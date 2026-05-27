# Dashboard giám sát và cảnh báo nguy cơ bệnh dại
# R Shiny + shinydashboard
# Tính năng chính:
# - Upload CSV/Excel, dùng dữ liệu mặc định hoặc đọc trực tiếp từ Google Sheets
# - KPI tổng quan
# - Phân loại nguy cơ Xanh - Vàng - Đỏ theo tỉnh/thành
# - Bảng dữ liệu có tô màu cảnh báo
# - Phân tích chi tiết theo địa phương

library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(plotly)
library(DT)
library(tidyr)
library(scales)
library(tools)
library(leaflet)

# =========================
# 1. Đường dẫn dữ liệu mặc định
# =========================
main_data_path <- "rabies_dashboard_data.csv"
detail_data_path <- "rabies_death_detail.csv"

# Link Google Sheets đã chia sẻ quyền "Bất kỳ ai có đường liên kết - Người xem".
# Google Sheets cần có đúng 2 tab: tong_hop và chi_tiet_tu_vong.
default_google_sheet_url <- "https://docs.google.com/spreadsheets/d/1E9IyKEi4LmmCnIC8Th1eIkeNPQGbfV1xvWAd01xsqPs/edit?usp=sharing"

if (!file.exists(main_data_path)) {
  stop("Chưa thấy file rabies_dashboard_data.csv trong cùng thư mục với app.R")
}

# =========================
# 2. Hàm đọc và làm sạch dữ liệu
# =========================
read_user_file <- function(path) {
  ext <- tolower(file_ext(path))
  if (ext == "csv") {
    read_csv(path, show_col_types = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    read_excel(path)
  } else {
    stop("Chỉ hỗ trợ file CSV hoặc Excel")
  }
}

read_google_sheet <- function(url, sheet_name) {
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    stop("Chưa cài gói googlesheets4. Hãy chạy: install.packages('googlesheets4')")
  }

  if (is.null(url) || trimws(url) == "") {
    stop("Chưa nhập link Google Sheets.")
  }

  googlesheets4::gs4_deauth()
  googlesheets4::read_sheet(url, sheet = sheet_name)
}

clean_main_data <- function(df) {
  df <- as.data.frame(df)
  required_cols <- c(
    "year", "month", "province", "region", "animal_bites",
    "vaccinated", "serum", "human_cases", "deaths"
  )

  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("Thiếu cột:", paste(missing_cols, collapse = ", ")))
  }

  for (col in c("year", "month", "animal_bites", "vaccinated", "serum", "human_cases", "deaths")) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    df[[col]][is.na(df[[col]])] <- 0
  }

  df$province <- as.character(df$province)
  df$region <- as.character(df$region)
  df
}

clean_detail_data <- function(df) {
  if (is.null(df)) return(NULL)

  df <- as.data.frame(df)
  if ("year" %in% names(df)) df$year <- suppressWarnings(as.numeric(df$year))
  if ("month" %in% names(df)) df$month <- suppressWarnings(as.numeric(df$month))
  if ("province" %in% names(df)) df$province <- as.character(df$province)
  if ("region" %in% names(df)) df$region <- as.character(df$region)
  if ("sex" %in% names(df)) df$sex <- as.character(df$sex)
  if ("age_group" %in% names(df)) df$age_group <- as.character(df$age_group)
  df
}

safe_rate <- function(num, den) {
  ifelse(is.na(den) | den == 0, 0, round(num / den * 100, 1))
}

rename_detail_cols <- function(df) {
  col_labels <- c(
    year = "Năm",
    month = "Tháng",
    province = "Tỉnh/thành",
    region = "Vùng",
    sex = "Giới tính",
    age_group = "Nhóm tuổi"
  )

  names(df) <- ifelse(
    names(df) %in% names(col_labels),
    unname(col_labels[names(df)]),
    names(df)
  )
  df
}

risk_color <- function(x) {
  dplyr::case_when(
    x == "Đỏ" ~ "#e74c3c",
    x == "Vàng" ~ "#f39c12",
    TRUE ~ "#00a65a"
  )
}

month_levels <- 1:12
month_labels <- paste0("T", 1:12)
risk_levels <- c("Đỏ", "Vàng", "Xanh")
risk_colors <- c("Đỏ" = "#e74c3c", "Vàng" = "#f39c12", "Xanh" = "#00a65a")
province_coords <- data.frame(
  province = c(
    "An Giang", "Bình Phước", "Bình Định", "Bắc Giang", "Cần Thơ",
    "Gia Lai", "Hà Nội", "Hải Phòng", "Hồ Chí Minh", "Khánh Hòa",
    "Kiên Giang", "Kon Tum", "Lâm Đồng", "Lạng Sơn", "Nghệ An",
    "Phú Yên", "Quảng Nam", "Quảng Ngãi", "Quảng Ninh", "Sơn La",
    "Thanh Hóa", "Tây Ninh", "Đà Nẵng", "Đắk Lắk", "Đồng Tháp"
  ),
  lat = c(
    10.5216, 11.7512, 13.7820, 21.2731, 10.0452,
    13.9718, 21.0285, 20.8449, 10.7769, 12.2388,
    10.0125, 14.3497, 11.9404, 21.8537, 18.6796,
    13.0882, 15.5736, 15.1214, 21.0064, 21.3270,
    19.8067, 11.3100, 16.0544, 12.6667, 10.4938
  ),
  lng = c(
    105.1259, 106.7235, 109.2190, 106.1946, 105.7469,
    108.0151, 105.8542, 106.6881, 106.7009, 109.1967,
    105.0809, 108.0005, 108.4583, 106.7615, 105.6813,
    109.0929, 108.4740, 108.8044, 107.2925, 103.9141,
    105.7852, 106.0983, 108.2022, 108.0382, 105.6882
  )
)

dt_lang_vi <- list(
  lengthMenu = "Hiển thị _MENU_ dòng",
  search = "Tìm kiếm:",
  info = "Hiển thị _START_ đến _END_ trong tổng số _TOTAL_ dòng",
  infoEmpty = "Không có dữ liệu",
  infoFiltered = "(lọc từ tổng số _MAX_ dòng)",
  zeroRecords = "Không tìm thấy dữ liệu phù hợp",
  emptyTable = "Không có dữ liệu trong bảng",
  paginate = list(
    previous = "Trước",
    `next` = "Sau"
  )
)

# =========================
# 3. Giao diện dashboard
# =========================
ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "Dashboard bệnh Dại"),

  dashboardSidebar(
    width = 270,
    sidebarMenu(
      menuItem("Tổng quan", tabName = "overview", icon = icon("chart-line")),
      menuItem("Cảnh báo nguy cơ", tabName = "risk", icon = icon("triangle-exclamation")),
      menuItem("Bản đồ dịch tễ", tabName = "map", icon = icon("map")),
      menuItem("Phân tích địa phương", tabName = "local", icon = icon("map-location-dot")),
      menuItem("Chi tiết tử vong", tabName = "detail", icon = icon("table"))
    ),
    br(),
    div(
      style = "padding: 0 15px;",
      p("Chọn nguồn dữ liệu để cập nhật dashboard."),
      radioButtons(
        "data_source",
        "Nguồn dữ liệu",
        choices = c(
          "Dữ liệu mặc định" = "default",
          "Upload CSV/Excel" = "upload",
          "Google Sheets" = "google"
        ),
        selected = "default"
      ),
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        fileInput("upload_main", "Tải file dữ liệu tổng hợp", accept = c(".csv", ".xlsx", ".xls")),
        fileInput("upload_detail", "Tải file dữ liệu chi tiết tử vong", accept = c(".csv", ".xlsx", ".xls"))
      ),
      conditionalPanel(
        condition = "input.data_source == 'google'",
        textInput("google_sheet_url", "Link Google Sheets", value = default_google_sheet_url),
        helpText("File Google Sheets cần có 2 tab: tong_hop và chi_tiet_tu_vong. Quyền chia sẻ nên để Bất kỳ ai có đường liên kết - Người xem.")
      ),
      uiOutput("year_ui"),
      uiOutput("region_ui")
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML("\n        .content-wrapper, .right-side { background-color: #f4f6f9; }\n        .main-header .logo { font-weight: 700; }\n        .box { border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); }\n        .box-header { border-top-left-radius: 8px; border-top-right-radius: 8px; }\n        .small-box { border-radius: 10px; }\n        .small-box h3 { font-size: 36px; font-weight: 700; }\n        .info-box { border-radius: 10px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); }\n        .dataTables_wrapper { font-size: 14px; }\n        .risk-note { padding: 10px 12px; border-left: 5px solid #e74c3c; background: #fff5f5; margin-bottom: 8px; border-radius: 5px; }\n        .legend-wrap { display: flex; gap: 12px; flex-wrap: wrap; }\n        .legend-card { flex: 1; min-width: 220px; padding: 12px 14px; border-radius: 8px; border-left: 7px solid #999; background: #ffffff; box-shadow: 0 1px 5px rgba(0,0,0,0.06); }\n        .legend-card strong { display: block; font-size: 16px; margin-bottom: 4px; }\n        .legend-red { border-left-color: #e74c3c; background: #fdecea; }\n        .legend-yellow { border-left-color: #f39c12; background: #fff8e1; }\n        .legend-green { border-left-color: #00a65a; background: #e8f5e9; }\n        .method-box { margin-top: 10px; padding: 10px 12px; border-radius: 8px; background: #f7f9fb; border: 1px solid #e1e5ea; }\n        .text-muted-small { color: #777; font-size: 13px; }\n      "))
    ),

    tabItems(
      # =========================
      # Tab 1: Tổng quan
      # =========================
      tabItem(
        tabName = "overview",

        fluidRow(
          valueBoxOutput("kpi_bites", width = 3),
          valueBoxOutput("kpi_vax", width = 3),
          valueBoxOutput("kpi_cases", width = 3),
          valueBoxOutput("kpi_deaths", width = 3)
        ),

        fluidRow(
          infoBoxOutput("kpi_red_provinces", width = 4),
          infoBoxOutput("kpi_vax_rate", width = 4),
          infoBoxOutput("kpi_fatality_rate", width = 4)
        ),

        fluidRow(
          box(
            width = 6,
            title = "Xu hướng theo tháng",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("trend_plot", height = 360)
          ),
          box(
            width = 6,
            title = "Top 10 tỉnh có tử vong cao",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("province_plot", height = 360)
          )
        ),

        fluidRow(
          box(
            width = 6,
            title = "Heatmap tử vong theo tháng",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("heatmap_plot", height = 430)
          ),
          box(
            width = 6,
            title = "Bảng dữ liệu tổng hợp",
            status = "info",
            solidHeader = TRUE,
            DTOutput("summary_table")
          )
        )
      ),

      # =========================
      # Tab 2: Cảnh báo nguy cơ
      # =========================
      tabItem(
        tabName = "risk",

        fluidRow(
          box(
            width = 12,
            title = "Chú giải mức cảnh báo",
            status = "danger",
            solidHeader = TRUE,
            div(
              class = "legend-wrap",
              div(
                class = "legend-card legend-red",
                strong("Đỏ - Nguy cơ cao"),
                span("Địa phương có tử vong/ca bệnh cao hoặc vượt ngưỡng cảnh báo. Cần ưu tiên theo dõi và can thiệp.")
              ),
              div(
                class = "legend-card legend-yellow",
                strong("Vàng - Cần theo dõi"),
                span("Địa phương có số ca hoặc tử vong ở mức trung bình/có dấu hiệu tăng. Cần tiếp tục giám sát.")
              ),
              div(
                class = "legend-card legend-green",
                strong("Xanh - Nguy cơ thấp"),
                span("Số ca nằm trong mức thấp hoặc bình thường so với dữ liệu hiện có.")
              )
            ),
            div(
              class = "method-box",
              strong("Cách xác định: "),
              span("Dashboard tính baseline tử vong từ các năm trước, tạo ngưỡng cảnh báo = baseline + 2 × độ lệch chuẩn, đồng thời kết hợp số ca bệnh và tử vong hiện tại để phân loại Xanh - Vàng - Đỏ.")
            )
          )
        ),

        fluidRow(
          box(
            width = 4,
            title = "Cơ cấu mức nguy cơ",
            status = "danger",
            solidHeader = TRUE,
            plotlyOutput("risk_donut", height = 330)
          ),
          box(
            width = 8,
            title = "Danh sách địa phương cần chú ý",
            status = "danger",
            solidHeader = TRUE,
            uiOutput("alert_list"),
            p(class = "text-muted-small", "Các địa phương được sắp xếp theo mức nguy cơ, số tử vong và số ca bệnh ở người.")
          )
        ),

        fluidRow(
          box(
            width = 12,
            title = "Bảng cảnh báo theo tỉnh/thành",
            status = "warning",
            solidHeader = TRUE,
            DTOutput("risk_table")
          )
        ),

        fluidRow(
          box(
            width = 12,
            title = "Xếp hạng nguy cơ theo tử vong và ca bệnh",
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("risk_bar", height = 440)
          )
        )
      ),
      
      # =========================
      # Tab 3: Bản đồ dịch tễ
      # =========================
      tabItem(
        tabName = "map",
        
        fluidRow(
          box(
            width = 12,
            title = "Bản đồ nguy cơ bệnh dại theo tỉnh/thành",
            status = "primary",
            solidHeader = TRUE,
            leafletOutput("risk_map", height = 620)
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Ghi chú",
            status = "info",
            solidHeader = TRUE,
            p("Mỗi điểm trên bản đồ đại diện cho một tỉnh/thành trong dữ liệu."),
            p("Màu sắc thể hiện mức nguy cơ: Đỏ = nguy cơ cao, Vàng = cần theo dõi, Xanh = nguy cơ thấp."),
            p("Kích thước điểm tăng theo số tử vong ghi nhận trong năm và vùng đang chọn.")
          )
        )
      ),

      # =========================
      # Tab 4: Phân tích địa phương
      # =========================
      tabItem(
        tabName = "local",

        fluidRow(
          box(
            width = 12,
            title = "Chọn địa phương để phân tích chi tiết",
            status = "primary",
            solidHeader = TRUE,
            uiOutput("local_province_ui")
          )
        ),

        fluidRow(
          infoBoxOutput("local_bites", width = 3),
          infoBoxOutput("local_vax", width = 3),
          infoBoxOutput("local_cases", width = 3),
          infoBoxOutput("local_deaths", width = 3)
        ),

        fluidRow(
          box(
            width = 7,
            title = "Xu hướng theo tháng của địa phương",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("local_trend_plot", height = 380)
          ),
          box(
            width = 5,
            title = "So sánh với trung bình vùng",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("local_compare_plot", height = 380)
          )
        ),

        fluidRow(
          box(
            width = 12,
            title = "Chi tiết tử vong tại địa phương",
            status = "warning",
            solidHeader = TRUE,
            DTOutput("local_detail_table")
          )
        )
      ),

      # =========================
      # Tab 5: Chi tiết tử vong
      # =========================
      tabItem(
        tabName = "detail",

        fluidRow(
          box(
            width = 6,
            title = "Tử vong theo giới",
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("sex_plot", height = 330)
          ),
          box(
            width = 6,
            title = "Tử vong theo nhóm tuổi",
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("age_plot", height = 330)
          )
        ),

        fluidRow(
          box(
            width = 12,
            title = "Bảng chi tiết tử vong",
            status = "warning",
            solidHeader = TRUE,
            DTOutput("detail_table")
          )
        )
      )
    )
  )
)

# =========================
# 4. Server
# =========================
server <- function(input, output, session) {

  default_main_df <- reactiveFileReader(
    intervalMillis = 5000,
    session = session,
    filePath = main_data_path,
    readFunc = function(path) read_csv(path, show_col_types = FALSE)
  )

  default_detail_df <- NULL
  if (file.exists(detail_data_path)) {
    default_detail_df <- reactiveFileReader(
      intervalMillis = 5000,
      session = session,
      filePath = detail_data_path,
      readFunc = function(path) read_csv(path, show_col_types = FALSE)
    )
  }

  main_df <- reactive({
    source <- input$data_source
    if (is.null(source)) source <- "default"

    df <- if (source == "google") {
      # Tự đọc lại Google Sheets sau mỗi 60 giây khi app đang mở.
      invalidateLater(60000, session)
      read_google_sheet(input$google_sheet_url, "tong_hop")
    } else if (source == "upload" && !is.null(input$upload_main)) {
      read_user_file(input$upload_main$datapath)
    } else {
      default_main_df()
    }

    clean_main_data(df)
  })

  detail_df <- reactive({
    source <- input$data_source
    if (is.null(source)) source <- "default"

    df <- if (source == "google") {
      invalidateLater(60000, session)
      read_google_sheet(input$google_sheet_url, "chi_tiet_tu_vong")
    } else if (source == "upload" && !is.null(input$upload_detail)) {
      read_user_file(input$upload_detail$datapath)
    } else if (!is.null(default_detail_df)) {
      default_detail_df()
    } else {
      NULL
    }

    clean_detail_data(df)
  })

  output$year_ui <- renderUI({
    req(main_df())
    years <- sort(unique(main_df()$year))
    selectInput("year", "Chọn năm", choices = years, selected = max(years))
  })

  output$region_ui <- renderUI({
    req(main_df())
    selectInput(
      "region",
      "Chọn vùng",
      choices = c("Tất cả", sort(unique(main_df()$region))),
      selected = "Tất cả"
    )
  })

  filtered_data <- reactive({
    req(input$year, input$region)
    df <- main_df() %>% filter(year == input$year)
    if (input$region != "Tất cả") {
      df <- df %>% filter(region == input$region)
    }
    df
  })

  filtered_detail <- reactive({
    df <- detail_df()
    if (is.null(df)) return(NULL)

    if ("year" %in% names(df)) {
      df <- df %>% filter(year == input$year)
    }
    if (input$region != "Tất cả" && "region" %in% names(df)) {
      df <- df %>% filter(region == input$region)
    }
    df
  })

  # Tổng hợp theo tỉnh + phân loại nguy cơ
  province_summary <- reactive({
    req(filtered_data(), input$year)

    current <- filtered_data() %>%
      group_by(province, region) %>%
      summarise(
        animal_bites = sum(animal_bites, na.rm = TRUE),
        vaccinated = sum(vaccinated, na.rm = TRUE),
        serum = sum(serum, na.rm = TRUE),
        human_cases = sum(human_cases, na.rm = TRUE),
        deaths = sum(deaths, na.rm = TRUE),
        .groups = "drop"
      )

    # Baseline: trung bình tử vong hằng năm của các năm trước cùng tỉnh.
    # Nếu năm đầu tiên không có baseline, dùng NA và dashboard vẫn chạy bình thường.
    baseline <- main_df() %>%
      filter(year < input$year) %>%
      group_by(year, province) %>%
      summarise(year_deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
      group_by(province) %>%
      summarise(
        baseline_deaths = round(mean(year_deaths, na.rm = TRUE), 1),
        baseline_sd = round(sd(year_deaths, na.rm = TRUE), 1),
        .groups = "drop"
      )

    current %>%
      left_join(baseline, by = "province") %>%
      mutate(
        baseline_sd = ifelse(is.na(baseline_sd), 0, baseline_sd),
        alert_threshold = ifelse(is.na(baseline_deaths), NA, round(baseline_deaths + 2 * baseline_sd, 1)),
        vax_rate = safe_rate(vaccinated, animal_bites),
        serum_rate = safe_rate(serum, animal_bites),
        fatality_rate = safe_rate(deaths, human_cases),
        baseline_alert = !is.na(alert_threshold) & deaths > alert_threshold & deaths >= 1,
        risk_level = case_when(
          deaths >= 10 | human_cases >= 20 | baseline_alert ~ "Đỏ",
          deaths >= 5 | human_cases >= 10 ~ "Vàng",
          TRUE ~ "Xanh"
        ),
        risk_level = factor(risk_level, levels = risk_levels),
        risk_score = case_when(
          risk_level == "Đỏ" ~ 3,
          risk_level == "Vàng" ~ 2,
          TRUE ~ 1
        )
      ) %>%
      arrange(desc(risk_score), desc(deaths), desc(human_cases))
  })

  local_selected <- reactive({
    ps <- province_summary()
    req(nrow(ps) > 0)
    if (!is.null(input$local_province) && input$local_province %in% ps$province) {
      input$local_province
    } else {
      ps$province[1]
    }
  })

  local_data <- reactive({
    req(local_selected())
    filtered_data() %>% filter(province == local_selected())
  })

  local_detail <- reactive({
    df <- filtered_detail()
    if (is.null(df)) return(NULL)
    if ("province" %in% names(df)) {
      df <- df %>% filter(province == local_selected())
    }
    df
  })

  output$local_province_ui <- renderUI({
    ps <- province_summary()
    selectInput(
      "local_province",
      "Chọn tỉnh/thành",
      choices = ps$province,
      selected = ps$province[1]
    )
  })
  
  # =========================
  # Tab bản đồ dịch tễ
  # =========================
  output$risk_map <- renderLeaflet({
    map_df <- province_summary() %>%
      left_join(province_coords, by = "province") %>%
      filter(!is.na(lat), !is.na(lng)) %>%
      mutate(
        risk_level_chr = as.character(risk_level),
        marker_radius = pmax(7, pmin(24, 7 + deaths)),
        popup_text = paste0(
          "<b>", province, "</b><br/>",
          "Vùng: ", region, "<br/>",
          "Người phơi nhiễm: ", comma(animal_bites), "<br/>",
          "Tiêm vắc xin: ", comma(vaccinated), "<br/>",
          "Ca bệnh ở người: ", comma(human_cases), "<br/>",
          "Tử vong: ", comma(deaths), "<br/>",
          "Tỷ lệ tiêm: ", vax_rate, "%<br/>",
          "Mức nguy cơ: <b>", risk_level_chr, "</b>"
        )
      )
    
    validate(
      need(nrow(map_df) > 0, "Không có dữ liệu tọa độ để hiển thị bản đồ.")
    )
    
    pal <- colorFactor(
      palette = unname(risk_colors),
      levels = risk_levels,
      domain = risk_levels
    )
    
    leaflet(map_df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 106.5, lat = 16.5, zoom = 5) %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        radius = ~marker_radius,
        color = ~pal(risk_level_chr),
        fillColor = ~pal(risk_level_chr),
        fillOpacity = 0.75,
        weight = 1.2,
        popup = ~popup_text,
        label = ~province
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~risk_level_chr,
        title = "Mức nguy cơ",
        opacity = 0.85
      )
  })
  
  # =========================
  # KPI tổng quan
  # =========================
  output$kpi_bites <- renderValueBox({
    valueBox(
      comma(sum(filtered_data()$animal_bites, na.rm = TRUE)),
      "Người phơi nhiễm",
      icon = icon("person-circle-exclamation"),
      color = "light-blue"
    )
  })

  output$kpi_vax <- renderValueBox({
    valueBox(
      comma(sum(filtered_data()$vaccinated, na.rm = TRUE)),
      "Tiêm vắc xin",
      icon = icon("syringe"),
      color = "green"
    )
  })

  output$kpi_cases <- renderValueBox({
    valueBox(
      comma(sum(filtered_data()$human_cases, na.rm = TRUE)),
      "Ca bệnh ở người",
      icon = icon("hospital"),
      color = "yellow"
    )
  })

  output$kpi_deaths <- renderValueBox({
    valueBox(
      comma(sum(filtered_data()$deaths, na.rm = TRUE)),
      "Tử vong",
      icon = icon("triangle-exclamation"),
      color = "red"
    )
  })

  output$kpi_red_provinces <- renderInfoBox({
    n_red <- province_summary() %>% filter(risk_level == "Đỏ") %>% nrow()
    infoBox(
      "Tỉnh nguy cơ cao",
      n_red,
      icon = icon("bell"),
      color = "red",
      fill = TRUE
    )
  })

  output$kpi_vax_rate <- renderInfoBox({
    rate <- safe_rate(sum(filtered_data()$vaccinated, na.rm = TRUE), sum(filtered_data()$animal_bites, na.rm = TRUE))
    infoBox(
      "Tỷ lệ tiêm vắc xin",
      paste0(rate, "%"),
      icon = icon("shield-heart"),
      color = "green",
      fill = TRUE
    )
  })

  output$kpi_fatality_rate <- renderInfoBox({
    rate <- safe_rate(sum(filtered_data()$deaths, na.rm = TRUE), sum(filtered_data()$human_cases, na.rm = TRUE))
    infoBox(
      "Tỷ lệ tử vong/ca bệnh",
      paste0(rate, "%"),
      icon = icon("chart-pie"),
      color = "yellow",
      fill = TRUE
    )
  })

  # =========================
  # Biểu đồ tab tổng quan
  # =========================
  output$trend_plot <- renderPlotly({
    plot_df <- filtered_data() %>%
      group_by(month) %>%
      summarise(
        human_cases = sum(human_cases, na.rm = TRUE),
        deaths = sum(deaths, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      complete(month = month_levels, fill = list(human_cases = 0, deaths = 0)) %>%
      arrange(month) %>%
      mutate(month_label = factor(paste0("T", month), levels = month_labels))

    p <- ggplot(plot_df, aes(x = month_label)) +
      geom_line(aes(y = human_cases, color = "Ca bệnh ở người", group = 1), linewidth = 1.2) +
      geom_point(aes(y = human_cases, color = "Ca bệnh ở người"), size = 2.6) +
      geom_line(aes(y = deaths, color = "Tử vong", group = 1), linewidth = 1.2, linetype = 2) +
      geom_point(aes(y = deaths, color = "Tử vong"), size = 2.6) +
      scale_color_manual(values = c("Ca bệnh ở người" = "#ff6f69", "Tử vong" = "#00bfc4")) +
      labs(x = "Tháng", y = "Số ca", color = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "top",
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.grid.minor = element_blank()
      )

    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      layout(legend = list(orientation = "h", x = 0.15, y = 1.12))
  })

  output$province_plot <- renderPlotly({
    plot_df <- province_summary() %>%
      arrange(desc(deaths), desc(human_cases)) %>%
      slice_head(n = 10) %>%
      mutate(province = reorder(province, deaths))

    p <- ggplot(plot_df, aes(x = province, y = deaths, fill = risk_level,
                             text = paste0(
                               "Tỉnh: ", province,
                               "<br>Tử vong: ", deaths,
                               "<br>Ca bệnh: ", human_cases,
                               "<br>Mức nguy cơ: ", risk_level
                             ))) +
      geom_col(width = 0.72) +
      coord_flip() +
      scale_fill_manual(values = risk_colors, drop = FALSE) +
      labs(x = NULL, y = "Số tử vong", fill = "Mức nguy cơ") +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank()
      )

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.15, y = 1.12))
  })

  output$heatmap_plot <- renderPlotly({
    top_provinces <- province_summary() %>%
      arrange(desc(deaths), desc(human_cases)) %>%
      slice_head(n = 15) %>%
      pull(province)

    plot_df <- filtered_data() %>%
      filter(province %in% top_provinces) %>%
      group_by(province, month) %>%
      summarise(deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
      complete(province = top_provinces, month = month_levels, fill = list(deaths = 0)) %>%
      left_join(province_summary() %>% select(province, total_deaths = deaths), by = "province") %>%
      mutate(
        month_label = factor(paste0("T", month), levels = month_labels),
        province = factor(province, levels = rev(top_provinces)),
        hover_text = paste0("Tỉnh: ", province, "<br>Tháng: ", month_label, "<br>Tử vong: ", deaths)
      )

    p <- ggplot(plot_df, aes(x = month_label, y = province, fill = deaths, text = hover_text)) +
      geom_tile(color = "white", linewidth = 0.25) +
      scale_fill_gradient(low = "#eaf3f8", high = "#e74c3c") +
      labs(x = "Tháng", y = NULL, fill = "Tử vong") +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.grid = element_blank()
      )

    ggplotly(p, tooltip = "text")
  })

  output$summary_table <- renderDT({
    table_df <- province_summary() %>%
      transmute(
        `Tỉnh` = province,
        `Vùng` = region,
        `Người phơi nhiễm` = animal_bites,
        `Tiêm vắc xin` = vaccinated,
        `Dùng huyết thanh` = serum,
        `Ca bệnh ở người` = human_cases,
        `Tử vong` = deaths,
        `Tỷ lệ tiêm (%)` = vax_rate,
        `Tỷ lệ tử vong (%)` = fatality_rate,
        `Mức nguy cơ` = as.character(risk_level)
      )

    datatable(
      table_df,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE, language = dt_lang_vi),
      filter = "top"
    ) %>%
      formatStyle(
        "Mức nguy cơ",
        target = "row",
        backgroundColor = styleEqual(
          c("Đỏ", "Vàng", "Xanh"),
          c("#fdecea", "#fff8e1", "#e8f5e9")
        )
      ) %>%
      formatRound(c("Tỷ lệ tiêm (%)", "Tỷ lệ tử vong (%)"), digits = 1)
  })

  # =========================
  # Tab cảnh báo nguy cơ
  # =========================
  output$risk_donut <- renderPlotly({
    plot_df <- province_summary() %>%
      count(risk_level, name = "n") %>%
      complete(risk_level = factor(risk_levels, levels = risk_levels), fill = list(n = 0))

    plot_ly(
      plot_df,
      labels = ~risk_level,
      values = ~n,
      type = "pie",
      hole = 0.55,
      marker = list(colors = risk_colors[as.character(plot_df$risk_level)]),
      textinfo = "label+value",
      hoverinfo = "label+value+percent"
    ) %>%
      layout(showlegend = TRUE)
  })

  output$alert_list <- renderUI({
    alerts <- province_summary() %>%
      filter(risk_level %in% c("Đỏ", "Vàng")) %>%
      arrange(desc(risk_score), desc(deaths), desc(human_cases)) %>%
      slice_head(n = 6)

    if (nrow(alerts) == 0) {
      return(div(class = "risk-note", strong("Không có địa phương nguy cơ cao trong bộ lọc hiện tại.")))
    }

    tagList(
      lapply(seq_len(nrow(alerts)), function(i) {
        color <- risk_colors[as.character(alerts$risk_level[i])]
        div(
          class = "risk-note",
          style = paste0("border-left-color:", color, ";"),
          strong(paste0(alerts$province[i], " - Mức ", alerts$risk_level[i])),
          br(),
          span(paste0(
            alerts$deaths[i], " tử vong, ",
            alerts$human_cases[i], " ca bệnh ở người, tỷ lệ tiêm ",
            alerts$vax_rate[i], "%"
          ))
        )
      })
    )
  })

  output$risk_table <- renderDT({
    table_df <- province_summary() %>%
      transmute(
        `Tỉnh` = province,
        `Vùng` = region,
        `Ca bệnh ở người` = human_cases,
        `Tử vong` = deaths,
        `Baseline tử vong` = baseline_deaths,
        `Ngưỡng cảnh báo` = alert_threshold,
        `Tỷ lệ tiêm (%)` = vax_rate,
        `Tỷ lệ dùng huyết thanh (%)` = serum_rate,
        `Tỷ lệ tử vong (%)` = fatality_rate,
        `Mức nguy cơ` = as.character(risk_level)
      )

    datatable(
      table_df,
      rownames = FALSE,
      options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE, language = dt_lang_vi),
      filter = "top"
    ) %>%
      formatStyle(
        "Mức nguy cơ",
        target = "row",
        backgroundColor = styleEqual(
          c("Đỏ", "Vàng", "Xanh"),
          c("#fdecea", "#fff8e1", "#e8f5e9")
        )
      ) %>%
      formatRound(c("Baseline tử vong", "Ngưỡng cảnh báo", "Tỷ lệ tiêm (%)", "Tỷ lệ dùng huyết thanh (%)", "Tỷ lệ tử vong (%)"), digits = 1)
  })

  output$risk_bar <- renderPlotly({
    plot_df <- province_summary() %>%
      arrange(desc(risk_score), desc(deaths), desc(human_cases)) %>%
      slice_head(n = 15) %>%
      mutate(province = reorder(province, deaths + human_cases * 0.2))

    p <- ggplot(plot_df, aes(x = province, y = deaths, fill = risk_level,
                             text = paste0(
                               "Tỉnh: ", province,
                               "<br>Vùng: ", region,
                               "<br>Ca bệnh: ", human_cases,
                               "<br>Tử vong: ", deaths,
                               "<br>Baseline tử vong: ", baseline_deaths,
                               "<br>Ngưỡng cảnh báo: ", alert_threshold,
                               "<br>Mức nguy cơ: ", risk_level
                             ))) +
      geom_col(width = 0.75) +
      coord_flip() +
      scale_fill_manual(values = risk_colors, drop = FALSE) +
      labs(x = NULL, y = "Số tử vong", fill = "Mức nguy cơ") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", panel.grid.minor = element_blank())

    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.15, y = 1.08))
  })

  # =========================
  # Tab phân tích địa phương
  # =========================
  output$local_bites <- renderInfoBox({
    infoBox(
      "Phơi nhiễm",
      comma(sum(local_data()$animal_bites, na.rm = TRUE)),
      icon = icon("person-circle-exclamation"),
      color = "light-blue",
      fill = TRUE
    )
  })

  output$local_vax <- renderInfoBox({
    rate <- safe_rate(sum(local_data()$vaccinated, na.rm = TRUE), sum(local_data()$animal_bites, na.rm = TRUE))
    infoBox(
      "Tỷ lệ tiêm",
      paste0(rate, "%"),
      icon = icon("syringe"),
      color = "green",
      fill = TRUE
    )
  })

  output$local_cases <- renderInfoBox({
    infoBox(
      "Ca bệnh",
      comma(sum(local_data()$human_cases, na.rm = TRUE)),
      icon = icon("hospital"),
      color = "yellow",
      fill = TRUE
    )
  })

  output$local_deaths <- renderInfoBox({
    local_risk <- province_summary() %>% filter(province == local_selected()) %>% pull(risk_level) %>% as.character()
    color <- ifelse(local_risk == "Đỏ", "red", ifelse(local_risk == "Vàng", "yellow", "green"))
    infoBox(
      "Tử vong",
      paste0(comma(sum(local_data()$deaths, na.rm = TRUE)), " - ", local_risk),
      icon = icon("triangle-exclamation"),
      color = color,
      fill = TRUE
    )
  })

  output$local_trend_plot <- renderPlotly({
    plot_df <- local_data() %>%
      group_by(month) %>%
      summarise(
        human_cases = sum(human_cases, na.rm = TRUE),
        deaths = sum(deaths, na.rm = TRUE),
        vaccinated = sum(vaccinated, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      complete(month = month_levels, fill = list(human_cases = 0, deaths = 0, vaccinated = 0)) %>%
      mutate(month_label = factor(paste0("T", month), levels = month_labels))

    p <- ggplot(plot_df, aes(x = month_label)) +
      geom_line(aes(y = human_cases, color = "Ca bệnh ở người", group = 1), linewidth = 1.2) +
      geom_point(aes(y = human_cases, color = "Ca bệnh ở người"), size = 2.4) +
      geom_line(aes(y = deaths, color = "Tử vong", group = 1), linewidth = 1.2, linetype = 2) +
      geom_point(aes(y = deaths, color = "Tử vong"), size = 2.4) +
      scale_color_manual(values = c("Ca bệnh ở người" = "#ff6f69", "Tử vong" = "#00bfc4")) +
      labs(x = "Tháng", y = "Số ca", color = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", panel.grid.minor = element_blank())

    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      layout(legend = list(orientation = "h", x = 0.15, y = 1.12))
  })

  output$local_compare_plot <- renderPlotly({
    selected <- province_summary() %>% filter(province == local_selected())
    req(nrow(selected) == 1)

    region_name <- selected$region[1]
    region_avg <- province_summary() %>%
      filter(region == region_name) %>%
      summarise(
        human_cases = mean(human_cases, na.rm = TRUE),
        deaths = mean(deaths, na.rm = TRUE),
        vax_rate = mean(vax_rate, na.rm = TRUE),
        .groups = "drop"
      )

    plot_df <- tibble::tibble(
      `Chỉ số` = c("Ca bệnh", "Tử vong", "Tỷ lệ tiêm (%)"),
      `Địa phương đang chọn` = c(selected$human_cases, selected$deaths, selected$vax_rate),
      `Trung bình vùng` = c(region_avg$human_cases, region_avg$deaths, region_avg$vax_rate)
    ) %>%
      pivot_longer(cols = c("Địa phương đang chọn", "Trung bình vùng"), names_to = "Nhóm", values_to = "Giá trị")

    p <- ggplot(plot_df, aes(x = `Chỉ số`, y = `Giá trị`, fill = Nhóm)) +
      geom_col(position = "dodge", width = 0.7) +
      labs(x = NULL, y = "Giá trị", fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", panel.grid.minor = element_blank())

    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(legend = list(orientation = "h", x = 0.05, y = 1.12))
  })

  output$local_detail_table <- renderDT({
    df <- local_detail()
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(
        data.frame(Thông_báo = "Không có bản ghi tử vong cho địa phương đang chọn."),
        rownames = FALSE,
        options = list(pageLength = 10, language = dt_lang_vi)
      ))
    }

    df <- rename_detail_cols(df)

    datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE, language = dt_lang_vi),
      filter = "top"
    )
  })

  # =========================
  # Tab chi tiết tử vong
  # =========================
  output$sex_plot <- renderPlotly({
    df <- filtered_detail()
    validate(
      need(!is.null(df), "Chưa có dữ liệu chi tiết tử vong."),
      need("sex" %in% names(df), "Dữ liệu chi tiết thiếu cột sex.")
    )

    plot_df <- df %>% count(sex, name = "n")

    p <- ggplot(plot_df, aes(x = sex, y = n, fill = sex)) +
      geom_col(width = 0.65, show.legend = FALSE) +
      labs(x = NULL, y = "Số tử vong") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())

    ggplotly(p, tooltip = c("x", "y"))
  })

  output$age_plot <- renderPlotly({
    df <- filtered_detail()
    validate(
      need(!is.null(df), "Chưa có dữ liệu chi tiết tử vong."),
      need("age_group" %in% names(df), "Dữ liệu chi tiết thiếu cột age_group.")
    )

    plot_df <- df %>%
      count(age_group, name = "n") %>%
      mutate(
        age_group_ordered = reorder(age_group, n),
        hover_text = paste0("Nhóm tuổi: ", age_group, "<br>Số tử vong: ", n)
      )

    p <- ggplot(plot_df, aes(x = age_group_ordered, y = n, text = hover_text)) +
      geom_col(fill = "#f39c12", width = 0.7) +
      coord_flip() +
      labs(x = "Nhóm tuổi", y = "Số tử vong") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())

    ggplotly(p, tooltip = "text")
  })

  output$detail_table <- renderDT({
    df <- filtered_detail()
    validate(need(!is.null(df), "Chưa có dữ liệu chi tiết tử vong."))

    df <- rename_detail_cols(df)

    datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE, language = dt_lang_vi),
      filter = "top"
    )
  })
}

shinyApp(ui, server)
