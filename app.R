# ------------------------------------------------------------------
# Occurrence Points Editor
# ------------------------------------------------------------------

# 1. PACOTES -------------------------------------------------------
pkgs <- c("shiny", "bslib", "leaflet", "DT", "dplyr")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
    stop("Por favor instale: install.packages(c('", paste(missing, collapse = "','"), "'))")
}

library(shiny)
library(bslib)
library(leaflet)
library(DT)
library(dplyr)

HAS_DRAW_TOOLS <- requireNamespace("leaflet.extras", quietly = TRUE)
if (HAS_DRAW_TOOLS) library(leaflet.extras)

options(shiny.maxRequestSize = 100 * 1024^2)
ZOOM_LEVEL <- 16

# 2. FUNÇÕES AUXILIARES --------------------------------------------
standardize <- function(df, col_lon, col_lat) {
    names(df)[names(df) == col_lon] <- "longitude"
    names(df)[names(df) == col_lat] <- "latitude"
    df$longitude <- suppressWarnings(as.numeric(df$longitude))
    df$latitude  <- suppressWarnings(as.numeric(df$latitude))
    if (!"status" %in% names(df)) df$status <- "unreviewed"
    df$id <- seq_len(nrow(df))
    others <- setdiff(names(df), c("id", "longitude", "latitude", "status"))
    df[, c("id", "longitude", "latitude", "status", others), drop = FALSE]
}

validate_rows <- function(df) {
    n <- nrow(df)
    if (n == 0) return(character(0))
    lon <- df$longitude; lat <- df$latitude
    
    no_coord <- is.na(lon) | is.na(lat)
    out_range <- !no_coord & (abs(lon) > 180 | abs(lat) > 90)
    zero      <- !no_coord & (lon == 0 & lat == 0)
    same      <- !no_coord & (lon == lat) & lon != 0
    
    vapply(seq_len(n), function(i) {
        p <- character(0)
        if (no_coord[i])        p <- c(p, "missing coordinate")
        if (isTRUE(out_range[i])) p <- c(p, "out of valid range")
        if (isTRUE(zero[i]))      p <- c(p, "lon/lat = 0")
        if (isTRUE(same[i]))      p <- c(p, "lon equals lat")
        paste(p, collapse = "; ")
    }, character(1))
}

sample_data <- function() {
    set.seed(42)
    n <- 30
    df <- data.frame(
        species   = sample(c("Panthera onca", "Chrysocyon brachyurus", "Tapirus terrestris"), n, TRUE),
        longitude = runif(n, -60, -40),
        latitude  = runif(n, -25, -5),
        year      = sample(1990:2024, n, TRUE),
        stringsAsFactors = FALSE
    )
    df$longitude[2] <- 0; df$latitude[2] <- 0
    df$latitude[5]  <- 95
    df[10, c("longitude", "latitude")] <- df[9, c("longitude", "latitude")]
    df$longitude[12] <- NA
    df
}

marker_color <- function(issue, status) {
    ifelse(status == "incorrect", "red",
           ifelse(status == "correct", "green",
                  ifelse(nzchar(issue), "red", "gray50")))
}

# Desenha um alfinete (Pin) em SVG codificado em Data URI para o Leaflet
svg_pin <- function(color) {
    color <- gsub("#", "%23", color) 
    svg <- paste0(
        "data:image/svg+xml;charset=UTF-8,",
        "%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' width='32' height='32'%3E",
        "%3Cpath d='M12 2C8.13 2 5 5.13 5 9c0 5.25 7 13 7 13s7-7.75 7-13c0-3.87-3.13-7-7-7zm0 9.5c-1.38 0-2.5-1.12-2.5-2.5s1.12-2.5 2.5-2.5 2.5 1.12 2.5 2.5-1.12 2.5-2.5 2.5z' ",
        "fill='", color, "' stroke='white' stroke-width='1'/%3E",
        "%3C/svg%3E"
    )
    return(svg)
}

point_in_polygon <- function(px, py, poly_x, poly_y) {
    n <- length(poly_x)
    inside <- rep(FALSE, length(px))
    j <- n
    for (i in seq_len(n)) {
        xi <- poly_x[i]; yi <- poly_y[i]
        xj <- poly_x[j]; yj <- poly_y[j]
        cond <- ((yi > py) != (yj > py)) &
            (px < (xj - xi) * (py - yi) / (yj - yi) + xi)
        cond[is.na(cond)] <- FALSE
        inside <- xor(inside, cond)
        j <- i
    }
    inside
}

# 3. INTERFACE (UI) ------------------------------------------------
ui <- page_sidebar(
    title = "Occurrence Points Editor",
    theme = bs_theme(version = 5, preset = "shiny",
                     primary = "#2563eb", 
                     success = "#2ecc71", 
                     danger = "#e74c3c",
                     secondary = "#6c757d") |>
        bs_add_rules("
      .btn { font-weight: 600; }
      .btn-primary { box-shadow: 0 0 8px rgba(37, 99, 235, 0.30); }
      .btn-success { box-shadow: 0 0 8px rgba(46, 204, 113, 0.30); }
      .btn-danger  { box-shadow: 0 0 8px rgba(231, 76, 60, 0.30); }
      .accordion-button:not(.collapsed) { color: #2563eb; }
      .card-header { font-weight: 600; }
    "),
    fillable = FALSE,
    
    sidebar = sidebar(
        width = 320,
        accordion(
            open = c("Data", "Actions"),
            
            accordion_panel(
                "Data", icon = icon("file-csv"),
                fileInput("file", "CSV file", accept = c(".csv", ".txt"),
                          buttonLabel = "Browse", placeholder = "no file selected"),
                layout_columns(
                    col_widths = c(6, 6),
                    selectInput("sep", "Separator", c("," = ",", ";" = ";", "tab" = "\t")),
                    selectInput("dec", "Decimal", c("." = ".", "," = ","))
                ),
                uiOutput("coord_pickers"),
                actionButton("load", "Load data", class = "btn-primary w-100 mb-2"),
                actionButton("demo", "Use sample data", class = "btn-outline-secondary w-100")
            ),
            
            accordion_panel(
                "Actions", icon = icon("pen-to-square"),
                actionButton("mark_ok", "Mark as correct", class = "btn-success w-100 mb-2"),
                actionButton("mark_bad", "Mark as incorrect", class = "btn-danger w-100 mb-2"),
                actionButton("undo", "Mark as unreviewed", class = "btn-secondary w-100 mb-2"),
                actionButton("zoom", "Zoom to selection", class = "btn-primary w-100 mb-2"),
                actionButton("select_all", "Select all", class = "btn-outline-secondary w-100 mb-2"),
                actionButton("deselect_all", "Deselect all", class = "btn-outline-secondary w-100 mb-2"),
                checkboxInput("needs_fix", "Show only records that still need correction", FALSE)
            ),
            
            accordion_panel(
                "Export", icon = icon("download"),
                downloadButton("download", "Download edited CSV", class = "btn-success w-100")
            )
        )
    ),
    
    card(
        card_header("Map"),
        leafletOutput("map", height = 460)
    ),
    
    card(
        card_header("Records"),
        DTOutput("table")
    )
)

# 4. SERVIDOR (SERVER) ---------------------------------------------
server <- function(input, output, session) {
    
    rv <- reactiveValues(
        df = data.frame(id = integer(0), longitude = numeric(0), latitude = numeric(0),
                        status = character(0), stringsAsFactors = FALSE),
        history = list(),
        raw = NULL,
        pinned_id = NULL
    )
    
    snapshot <- function() {
        rv$history <- utils::head(c(list(rv$df), rv$history), 20)
    }
    
    observeEvent(input$file, {
        rv$raw <- utils::read.csv(input$file$datapath, sep = input$sep, dec = input$dec,
                                  stringsAsFactors = FALSE, check.names = TRUE)
    })
    
    output$coord_pickers <- renderUI({
        req(rv$raw)
        cols <- names(rv$raw)
        guess <- function(p) {
            hit <- cols[grepl(p, cols, ignore.case = TRUE)]
            if (length(hit)) hit[1] else cols[1]
        }
        tagList(
            selectInput("col_lon", "Longitude column", cols, selected = guess("^lon|^x$|decimalLong")),
            selectInput("col_lat", "Latitude column",  cols, selected = guess("^lat|^y$|decimalLat"))
        )
    })
    
    observeEvent(input$load, {
        req(rv$raw, input$col_lon, input$col_lat)
        if (input$col_lon == input$col_lat) {
            showNotification("Longitude and latitude must be different columns.", type = "error")
            return()
        }
        rv$df <- standardize(rv$raw, input$col_lon, input$col_lat)
        rv$pinned_id <- NULL
        showNotification(paste(nrow(rv$df), "records loaded."), type = "message")
    })
    
    observeEvent(input$demo, {
        rv$df <- standardize(sample_data(), "longitude", "latitude")
        rv$pinned_id <- NULL
        showNotification("Sample data loaded.", type = "message")
    })
    
    df_diag <- reactive({
        d <- rv$df
        if (nrow(d) == 0) return(cbind(d, issue = character(0)))
        d$issue <- validate_rows(d)
        d
    })
    
    df_visible <- reactive({
        d <- df_diag()
        if (isTRUE(input$needs_fix) && nrow(d) > 0) {
            d <- d[d$status %in% c("unreviewed", "edited"), ]
        }
        if (!is.null(rv$pinned_id) && nrow(d) > 0 && rv$pinned_id %in% d$id) {
            idx <- which(d$id == rv$pinned_id)
            d <- rbind(d[idx, , drop = FALSE], d[-idx, , drop = FALSE])
        }
        d
    })
    
    selected_ids <- reactive({
        d <- df_visible()
        sel <- input$table_rows_selected
        if (is.null(sel) || nrow(d) == 0) integer(0) else d$id[sel]
    })
    
    output$map <- renderLeaflet({
        m <- leaflet() |>
            addProviderTiles(providers$OpenStreetMap, group = "Street") |>
            addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
            addLayersControl(baseGroups = c("Street", "Satellite"),
                             options = layersControlOptions(collapsed = TRUE))
        if (HAS_DRAW_TOOLS) {
            m <- m |> leaflet.extras::addDrawToolbar(
                targetGroup = "drawnPoly",
                polylineOptions = FALSE, circleOptions = FALSE, circleMarkerOptions = FALSE,
                markerOptions = FALSE,
                rectangleOptions = leaflet.extras::drawRectangleOptions(),
                polygonOptions = leaflet.extras::drawPolygonOptions(),
                editOptions = leaflet.extras::editToolbarOptions(edit = FALSE, remove = TRUE)
            )
        }
        m |> setView(lng = -50, lat = -15, zoom = 4)
    })
    
    observe({
        d <- df_visible()
        sel_ids <- selected_ids()
        proxy <- leafletProxy("map") |> clearGroup("points") |> clearGroup("highlight")
        
        ok <- d[!is.na(d$longitude) & !is.na(d$latitude) &
                    abs(d$longitude) <= 180 & abs(d$latitude) <= 90, ]
        
        if (nrow(ok) == 0) return()
        
        # Gera os ícones SVG dinâmicos no formato de alfinete (pin)
        colors <- marker_color(ok$issue, ok$status)
        pin_urls <- vapply(colors, svg_pin, character(1), USE.NAMES = FALSE)
        
        my_icons <- icons(
            iconUrl = pin_urls,
            iconWidth = 32, iconHeight = 32,
            iconAnchorX = 16, iconAnchorY = 32,
            popupAnchorX = 0, popupAnchorY = -32
        )
        
        proxy |> addMarkers(
            lng = ok$longitude, lat = ok$latitude,
            layerId = as.character(ok$id), group = "points",
            icon = my_icons,
            label = paste0("id ", ok$id),
            popup = paste0("<b>id ", ok$id, "</b><br>",
                           "lon: ", round(ok$longitude, 5),
                           "<br>lat: ", round(ok$latitude, 5),
                           ifelse(nzchar(ok$issue),
                                  paste0("<br><span style='color:red'>", ok$issue, "</span>"), ""))
        )
        
        sel <- ok[ok$id %in% sel_ids, ]
        if (nrow(sel) > 0) {
            proxy |> addCircleMarkers(
                lng = sel$longitude, lat = sel$latitude, group = "highlight",
                radius = 16, color = "#FFC107", weight = 4, opacity = 1, fillOpacity = 0
            )
        }
    })
    
    observeEvent(input$map_marker_click, {
        id <- suppressWarnings(as.integer(input$map_marker_click$id))
        req(!is.na(id))
        rv$pinned_id <- id
    })
    
    observeEvent(input$map_draw_new_feature, {
        req(HAS_DRAW_TOOLS)
        feat <- input$map_draw_new_feature
        req(identical(feat$geometry$type, "Polygon"))
        ring <- feat$geometry$coordinates[[1]]
        poly_mat <- do.call(rbind, lapply(ring, unlist))
        poly_lon <- poly_mat[, 1]; poly_lat <- poly_mat[, 2]
        
        d <- df_visible()
        ok <- d[!is.na(d$longitude) & !is.na(d$latitude), ]
        req(nrow(ok) > 0)
        inside <- point_in_polygon(ok$longitude, ok$latitude, poly_lon, poly_lat)
        ids_in <- ok$id[inside]
        
        rv$pinned_id <- NULL
        row_idx <- which(d$id %in% ids_in)
        DT::selectRows(DT::dataTableProxy("table"), row_idx)
        leafletProxy("map") |> clearGroup("drawnPoly")
        showNotification(paste(length(ids_in), "point(s) selected."), type = "message")
    })
    
    observeEvent(df_visible(), {
        if (!is.null(rv$pinned_id)) {
            d <- df_visible()
            row <- which(d$id == rv$pinned_id)
            if (length(row) == 1) DT::selectRows(DT::dataTableProxy("table"), row)
        }
    })
    
    output$table <- renderDT({
        d <- df_visible()
        datatable(
            d, rownames = FALSE, selection = "multiple",
            editable = list(target = "cell", disable = list(columns = c(0, ncol(d) - 1))),
            options = list(pageLength = 10, scrollX = TRUE, dom = "ltip",
                           columnDefs = list(list(className = "dt-center", targets = 0)))
        ) |>
            formatStyle("issue", target = "row",
                        backgroundColor = styleEqual("", "white", default = "#fdecea"))
    }, server = TRUE)
    
    observeEvent(input$table_cell_edit, {
        info <- input$table_cell_edit
        d <- df_visible()
        id <- d$id[info$row]
        column <- names(d)[info$col + 1]
        if (column %in% c("id", "issue")) return()
        snapshot()
        i <- which(rv$df$id == id)
        value <- info$value
        if (column %in% c("longitude", "latitude")) {
            value <- suppressWarnings(as.numeric(gsub(",", ".", value)))
            limit <- if (column == "longitude") 180 else 90
            if (!is.na(value) && abs(value) > limit) {
                showNotification(paste0(column, " is outside the valid range (±", limit, ")."),
                                 type = "warning")
            }
            if (identical(rv$df$status[i], "unreviewed")) rv$df$status[i] <- "edited"
        } else if (is.numeric(rv$df[[column]])) {
            value <- suppressWarnings(as.numeric(value))
        } else {
            value <- as.character(value)
        }
        rv$df[i, column] <- value
    })
    
    set_status <- function(new_status) {
        ids <- selected_ids()
        if (!length(ids)) {
            showNotification("Select at least one row.", type = "warning"); return()
        }
        snapshot()
        rv$df$status[rv$df$id %in% ids] <- new_status
    }
    
    observeEvent(input$mark_ok,  set_status("correct"))
    observeEvent(input$mark_bad, set_status("incorrect"))
    observeEvent(input$undo,     set_status("unreviewed"))
    
    observeEvent(input$select_all, {
        rv$pinned_id <- NULL
        n_rows <- nrow(df_visible())
        if (n_rows > 0) {
            DT::selectRows(DT::dataTableProxy("table"), seq_len(n_rows))
        }
    })
    
    observeEvent(input$deselect_all, {
        rv$pinned_id <- NULL
        DT::selectRows(DT::dataTableProxy("table"), NULL)
    })
    
    observeEvent(input$zoom, {
        ids <- selected_ids()
        if (!length(ids)) {
            showNotification("Select at least one row.", type = "warning"); return()
        }
        d <- rv$df[rv$df$id %in% ids, ]
        d <- d[!is.na(d$longitude) & !is.na(d$latitude), ]
        req(nrow(d) > 0)
        if (nrow(d) == 1) {
            leafletProxy("map") |> setView(d$longitude, d$latitude, zoom = ZOOM_LEVEL)
        } else {
            leafletProxy("map") |> fitBounds(min(d$longitude), min(d$latitude),
                                             max(d$longitude), max(d$latitude))
        }
    })
    
    observeEvent(input$map_marker_click, {
        ev <- input$map_marker_click
        leafletProxy("map") |> setView(ev$lng, ev$lat, zoom = ZOOM_LEVEL)
    })
    
    output$download <- downloadHandler(
        filename = function() paste0("occurrences_edited_", Sys.Date(), ".csv"),
        content = function(file) {
            utils::write.csv(df_diag(), file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
        }
    )
}

# 5. EXECUÇÃO DO APP -----------------------------------------------
shinyApp(ui = ui, server = server)