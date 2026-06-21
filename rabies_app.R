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

month_levels <- 1:12
month_labels <- paste0("T", 1:12)
risk_levels <- c("Đỏ", "Vàng", "Xanh")
risk_colors <- c("Đỏ" = "#ef4444", "Vàng" = "#f59e0b", "Xanh" = "#10b981")

# =========================
# 2.5 Hàm cấu hình giao diện Premium
# =========================
premium_theme <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "Inter") +
    theme(
      legend.position = "top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "#64748b", face = "bold"),
      axis.title = element_text(color = "#475569", face = "bold"),
      plot.title = element_text(color = "#1e293b", face = "bold"),
      strip.text = element_text(face = "bold", size = base_size)
    )
}

premium_plotly_layout <- function(p, showlegend = TRUE, orientation = "h", lx = 0, ly = 1.15) {
  lyt <- p %>% layout(
    plot_bgcolor = "rgba(0,0,0,0)",
    paper_bgcolor = "rgba(0,0,0,0)",
    font = list(family = "Inter, sans-serif")
  )
  if (showlegend) {
    lyt <- lyt %>% layout(legend = list(orientation = orientation, x = lx, y = ly))
  } else {
    lyt <- lyt %>% layout(showlegend = FALSE)
  }
  lyt
}
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
      menuItem("Chi tiết tử vong", tabName = "detail", icon = icon("table")),
      menuItem("Dự báo & Đánh giá mô hình", tabName = "forecast", icon = icon("chart-line"))
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
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');
        
        /* Global Font */
        body, h1, h2, h3, h4, h5, h6, .main-header .logo, .main-sidebar, .selectize-input, .btn, .form-control {
          font-family: 'Inter', sans-serif !important;
        }
        
        /* Background & Layout */
        .content-wrapper, .right-side { background-color: #f8fafc; }
        
        /* Header Premium Light */
        .skin-blue .main-header .navbar { background-color: #ffffff; border-bottom: 1px solid #e2e8f0; }
        .skin-blue .main-header .logo { background-color: #ffffff; color: #1e293b; border-bottom: 1px solid #e2e8f0; border-right: 1px solid #e2e8f0; font-weight: 700; }
        .skin-blue .main-header .logo:hover { background-color: #f8fafc; color: #0f172a; }
        .skin-blue .main-header .navbar .sidebar-toggle { color: #64748b; }
        .skin-blue .main-header .navbar .sidebar-toggle:hover { background-color: #f1f5f9; color: #0f172a; }
        
        /* Sidebar Premium Light */
        .skin-blue .sidebar .radio label, .skin-blue .sidebar .checkbox label { color: #475569 !important; font-weight: 500; }
        .skin-blue .sidebar .control-label { color: #334155 !important; font-weight: 600; }

        .skin-blue .main-sidebar { background-color: #ffffff; border-right: 1px solid #e2e8f0; }
        .skin-blue .sidebar a { color: #475569; font-weight: 500; transition: all 0.2s; }
        .skin-blue .sidebar-menu > li.active > a, .skin-blue .sidebar-menu > li:hover > a { background-color: #f1f5f9; color: #0f172a; border-left-color: #3b82f6; }
        .skin-blue .sidebar-menu > li > a { border-left: 3px solid transparent; }
        .skin-blue .wrapper, .skin-blue .main-sidebar, .skin-blue .left-side { background-color: #ffffff; }
        .control-label { color: #334155; font-weight: 600; font-size: 13px; margin-top: 10px; }
        
        /* Scrollbar */
        ::-webkit-scrollbar { width: 6px; height: 6px; }
        ::-webkit-scrollbar-track { background: transparent; }
        ::-webkit-scrollbar-thumb { background: #cbd5e1; border-radius: 4px; }
        ::-webkit-scrollbar-thumb:hover { background: #94a3b8; }
        
        /* Inputs & Form Controls */
        .form-control { border-radius: 8px; border: 1px solid #cbd5e1; box-shadow: none; padding: 10px 12px; height: auto; transition: border-color 0.2s, box-shadow 0.2s; }
        .form-control:focus { border-color: #3b82f6; box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1); }
        .selectize-input { border-radius: 8px; border: 1px solid #cbd5e1; box-shadow: none; padding: 10px 12px; }
        .selectize-input.focus { border-color: #3b82f6; box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1); }
        .selectize-dropdown { border-radius: 8px; border: 1px solid #e2e8f0; box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1); font-size: 14px; }
        .btn-file { border-radius: 8px; font-weight: 500; padding: 8px 16px; background-color: #f1f5f9; color: #334155; border: 1px solid #cbd5e1; }
        .btn-file:hover { background-color: #e2e8f0; }
        
        /* Box / Cards */
        .box { border-radius: 12px; box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.05), 0 2px 4px -1px rgba(0, 0, 0, 0.03); border: 1px solid #e2e8f0; background: #ffffff; margin-bottom: 24px; transition: transform 0.2s ease, box-shadow 0.2s ease; border-top: none; }
        .box:hover { transform: translateY(-2px); box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.08); }
        .box-header { border-top-left-radius: 12px; border-top-right-radius: 12px; padding: 16px 20px; border-bottom: 1px solid #f1f5f9; }
        .box-header .box-title { font-size: 16px; font-weight: 600; color: #1e293b; }
        .box-body { padding: 20px; }
        
        /* Small Box (KPI) */
        .small-box { border-radius: 12px; box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.05); background: #ffffff !important; color: #1e293b !important; border: 1px solid #e2e8f0; overflow: hidden; transition: transform 0.2s ease; margin-bottom: 24px; position: relative; }
        .small-box:hover { transform: translateY(-3px); box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.08); }
        .small-box .inner { padding: 20px; position: relative; z-index: 2; }
        .small-box h3 { font-size: 32px; font-weight: 700; color: #0f172a; margin: 0 0 5px 0; }
        .small-box p { font-size: 13px; color: #64748b; font-weight: 600; margin: 0; text-transform: uppercase; letter-spacing: 0.5px; }
        .small-box .icon { position: absolute; right: 20px; top: 10px; font-size: 60px; color: rgba(0,0,0,0.03); z-index: 1; transition: transform 0.3s ease; }
        .small-box:hover .icon { transform: scale(1.1) rotate(5deg); }
        
        /* Accents for Small Boxes */
        .small-box.bg-aqua, .small-box.bg-light-blue { border-bottom: 4px solid #3b82f6 !important; } .small-box.bg-aqua .icon, .small-box.bg-light-blue .icon { color: #eff6ff !important; }
        .small-box.bg-green { border-bottom: 4px solid #10b981 !important; } .small-box.bg-green .icon { color: #ecfdf5 !important; }
        .small-box.bg-yellow { border-bottom: 4px solid #f59e0b !important; } .small-box.bg-yellow .icon { color: #fffbeb !important; }
        .small-box.bg-red { border-bottom: 4px solid #ef4444 !important; } .small-box.bg-red .icon { color: #fef2f2 !important; }
        .small-box.bg-purple { border-bottom: 4px solid #a855f7 !important; } .small-box.bg-purple .icon { color: #faf5ff !important; }
        
        /* Accents for Info Boxes */
        .info-box-icon.bg-red { background-color: #fee2e2 !important; color: #ef4444 !important; }
        .info-box-icon.bg-green { background-color: #d1fae5 !important; color: #10b981 !important; }
        .info-box-icon.bg-yellow { background-color: #fef3c7 !important; color: #f59e0b !important; }
        .info-box-icon.bg-aqua, .info-box-icon.bg-light-blue { background-color: #e0f2fe !important; color: #3b82f6 !important; }
        .info-box-icon.bg-purple { background-color: #f3e8ff !important; color: #a855f7 !important; }
        
        /* Info Box */
        .info-box { border-radius: 12px; box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.05); border: 1px solid #e2e8f0; background: #ffffff; min-height: 80px; transition: transform 0.2s ease; margin-bottom: 24px; }
        .info-box:hover { transform: translateY(-3px); box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.08); }
        .info-box-icon { border-top-left-radius: 12px; border-bottom-left-radius: 12px; width: 80px; height: 80px; line-height: 80px; font-size: 32px; }
        .info-box-content { padding: 15px 20px; margin-left: 80px; }
        .info-box-text { font-size: 13px; color: #64748b; font-weight: 600; text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 4px; }
        .info-box-number { font-size: 24px; font-weight: 700; color: #0f172a; }
        
        /* DataTables */
        .dataTables_wrapper { font-size: 14px; }
        table.dataTable { border-collapse: collapse !important; border: none !important; }
        table.dataTable thead th { border-bottom: 2px solid #e2e8f0 !important; color: #475569; font-weight: 600; text-transform: uppercase; font-size: 12px; letter-spacing: 0.5px; padding: 12px 10px !important; }
        table.dataTable tbody td { border-bottom: 1px solid #f1f5f9 !important; padding: 12px 10px !important; vertical-align: middle; }
        table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover { background-color: #f8fafc !important; }
        
        /* Legend & Notes */
        .risk-note { padding: 14px 16px; border-left: 4px solid #ef4444; background: #fef2f2; margin-bottom: 24px; border-radius: 8px; color: #991b1b; font-weight: 500; font-size: 14px; }
        .legend-wrap { display: flex; gap: 16px; flex-wrap: wrap; margin-bottom: 24px; }
        .legend-card { flex: 1; min-width: 200px; padding: 16px; border-radius: 12px; border-left: 6px solid #999; background: #ffffff; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.05); border: 1px solid #e2e8f0; border-left-width: 6px; }
        .legend-card strong { display: block; font-size: 15px; font-weight: 700; margin-bottom: 6px; color: #1e293b; }
        .legend-card span { font-size: 13px; color: #64748b; }
        .legend-red { border-left-color: #ef4444; } .legend-red strong { color: #b91c1c; }
        .legend-yellow { border-left-color: #f59e0b; } .legend-yellow strong { color: #b45309; }
        .legend-green { border-left-color: #10b981; } .legend-green strong { color: #047857; }
        
        .method-box { margin-top: 16px; padding: 16px; border-radius: 8px; background: #f8fafc; border: 1px solid #e2e8f0; }
        .text-muted-small { color: #64748b; font-size: 13px; line-height: 1.6; }
      "))
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
            width = 4,
            title = "Xu hướng theo tháng",
            plotlyOutput("trend_plot", height = 360)
          ),
          box(
            width = 4,
            title = "Top 10 tỉnh có tử vong cao",
            plotlyOutput("province_plot", height = 360)
          ),
          box(
            width = 4,
            title = "Tỷ lệ tiêm vắc xin theo tháng",
            plotlyOutput("vax_trend_plot", height = 360)
          )
        ),

        fluidRow(
          box(
            width = 12,
            title = "Heatmap tử vong theo tháng",
            plotlyOutput("heatmap_plot", height = 430)
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
            plotlyOutput("risk_donut", height = 330)
          ),
          box(
            width = 8,
            title = "Danh sách địa phương cần chú ý",
            uiOutput("alert_list"),
            p(class = "text-muted-small", "Các địa phương được sắp xếp theo mức nguy cơ, số tử vong và số ca bệnh ở người.")
          )
        ),

        fluidRow(
          box(
            width = 12,
            title = "Bảng cảnh báo theo tỉnh/thành",
            DTOutput("risk_table")
          )
        ),

        fluidRow(
          box(
            width = 12,
            title = "Xếp hạng nguy cơ theo tử vong và ca bệnh",
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
            leafletOutput("risk_map", height = 620)
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Ghi chú",
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
            uiOutput("local_province_ui")
          )
        ),

        fluidRow(
          infoBoxOutput("local_bites", width = 4),
          infoBoxOutput("local_vax", width = 4),
          infoBoxOutput("local_serum", width = 4)
        ),

        fluidRow(
          infoBoxOutput("local_cases", width = 6),
          infoBoxOutput("local_deaths", width = 6)
        ),

        fluidRow(
          box(
            width = 7,
            title = "Xu hướng theo tháng của địa phương",
            plotlyOutput("local_trend_plot", height = 380)
          ),
          box(
            width = 5,
            title = "So sánh với trung bình vùng",
            plotlyOutput("local_compare_plot", height = 380)
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Tương quan giữa tỷ lệ tiêm và tỷ lệ tử vong",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("correlation_plot", height = 400)
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
            plotlyOutput("sex_plot", height = 330)
          ),
          box(
            width = 6,
            title = "Tử vong theo nhóm tuổi",
            plotlyOutput("age_plot", height = 330)
          )
        ),

        fluidRow(
          box(
            width = 12,
            title = "Bảng chi tiết tử vong",
            DTOutput("detail_table")
          )
        )
      ),
      
      # =========================
      # Tab 6: Dự báo & Đánh giá
      # =========================
      tabItem(
        tabName = "forecast",
        fluidRow(
          box(width = 12, title = "Chọn tỉnh để dự báo", status = "primary", solidHeader = TRUE,
              selectizeInput("forecast_province", "Chọn tỉnh/thành", 
                             choices = sort(unique(province_coords$province)), 
                             selected = "Nghệ An",
                             options = list(dropdownParent = "body"))
          )
        ),
        fluidRow(
          box(width = 12, title = "Dự báo xu hướng ca bệnh và tử vong", status = "primary", solidHeader = TRUE,
              plotlyOutput("forecast_plot", height = 450),
              uiOutput("forecast_legend")
          )
        ),
        fluidRow(
          box(width = 12, title = "Đánh giá mô hình dự báo (kiểm định trên năm gần nhất)", status = "info", solidHeader = TRUE,
              uiOutput("forecast_metrics")
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
        baseline_ci_lower = baseline_deaths - 1.96 * (baseline_sd / sqrt(5)),
        baseline_ci_upper = baseline_deaths + 1.96 * (baseline_sd / sqrt(5)),
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

  output$local_province_ui <- renderUI({
    ps <- province_summary()
    selectizeInput(
      "local_province",
      "Chọn tỉnh/thành",
      choices = ps$province,
      selected = ps$province[1],
      options = list(dropdownParent = "body")
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
          "KTC 95%: ", round(baseline_ci_lower, 1), " – ", round(baseline_ci_upper, 1), "<br/>",
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
        stroke = TRUE,
        weight = 2,
        opacity = 0.9,
        fillColor = ~pal(risk_level_chr),
        fillOpacity = 0.4,
        popup = ~popup_text,
        label = ~province,
        labelOptions = labelOptions(
          style = list('font-weight' = '500', padding = '4px 8px', 'border-radius' = '6px'),
          textsize = '13px',
          direction = 'auto'
        )
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
      color = "red")
  })

  output$kpi_vax_rate <- renderInfoBox({
    rate <- safe_rate(sum(filtered_data()$vaccinated, na.rm = TRUE), sum(filtered_data()$animal_bites, na.rm = TRUE))
    infoBox(
      "Tỷ lệ tiêm vắc xin",
      paste0(rate, "%"),
      icon = icon("shield-heart"),
      color = "green")
  })

  output$kpi_fatality_rate <- renderInfoBox({
    rate <- safe_rate(sum(filtered_data()$deaths, na.rm = TRUE), sum(filtered_data()$human_cases, na.rm = TRUE))
    infoBox(
      "Tỷ lệ tử vong/ca bệnh",
      paste0(rate, "%"),
      icon = icon("chart-pie"),
      color = "yellow")
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
    
    plot_ly(plot_df, x = ~month_label) %>%
      add_trace(
        y = ~human_cases, name = "Ca bệnh ở người", 
        type = "scatter", mode = "lines+markers",
        line = list(color = "#f59e0b", width = 3, shape = "spline", smoothing = 1.3),
        marker = list(color = "#f59e0b", size = 8),
        fill = "tozeroy", fillcolor = "rgba(245, 158, 11, 0.1)",
        hovertemplate = "<b>%{x}</b><br>Ca bệnh: %{y}<extra></extra>"
      ) %>%
      add_trace(
        y = ~deaths, name = "Tử vong", 
        type = "scatter", mode = "lines+markers",
        line = list(color = "#ef4444", width = 3, shape = "spline", smoothing = 1.3),
        marker = list(color = "#ef4444", size = 8),
        fill = "tozeroy", fillcolor = "rgba(239, 68, 68, 0.1)",
        hovertemplate = "<b>%{x}</b><br>Tử vong: %{y}<extra></extra>"
      ) %>%
      layout(
        xaxis = list(title = "", showgrid = FALSE, tickfont = list(color = "#64748b", family = "Inter")),
        yaxis = list(title = "Số ca", showgrid = TRUE, gridcolor = "#f1f5f9", zeroline = FALSE, tickfont = list(color = "#64748b", family = "Inter")),
        hovermode = "x unified"
      ) %>% premium_plotly_layout(lx = 0.15, ly = 1.12)
  })
  
  output$vax_trend_plot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    vax_by_month <- df %>%
      group_by(month) %>%
      summarise(
        total_bites = sum(animal_bites, na.rm = TRUE),
        total_vaccinated = sum(vaccinated, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        vax_rate = safe_rate(total_vaccinated, total_bites)
      ) %>%
      complete(month = month_levels, fill = list(vax_rate = 0)) %>%
      mutate(month_label = factor(paste0("T", month), levels = month_labels))
    
    plot_ly(vax_by_month, x = ~month_label) %>%
      add_trace(
        y = ~vax_rate, name = "Tỷ lệ tiêm",
        type = "scatter", mode = "lines+markers",
        line = list(color = "#3b82f6", width = 3, shape = "spline", smoothing = 1.3),
        marker = list(color = "#1d4ed8", size = 8),
        fill = "tozeroy", fillcolor = "rgba(59, 130, 246, 0.15)",
        hovertemplate = "<b>%{x}</b><br>Tỷ lệ tiêm: %{y}%<extra></extra>"
      ) %>%
      layout(
        xaxis = list(title = "", showgrid = FALSE, tickfont = list(color = "#64748b", family = "Inter")),
        yaxis = list(title = "Tỷ lệ tiêm (%)", range = c(0, 100), showgrid = TRUE, gridcolor = "#f1f5f9", zeroline = FALSE, tickfont = list(color = "#64748b", family = "Inter")),
        hovermode = "closest"
      ) %>% premium_plotly_layout(showlegend = FALSE)
  })

  output$province_plot <- renderPlotly({
    plot_df <- province_summary() %>%
      arrange(desc(deaths), desc(human_cases)) %>%
      slice_head(n = 10) %>%
      mutate(province = factor(province, levels = rev(unique(province))))

    plot_ly(plot_df, y = ~province, x = ~deaths, type = "bar", orientation = "h",
            color = ~risk_level, colors = risk_colors,
            text = ~paste0("Tỉnh: ", province, "<br>Tử vong: ", deaths, "<br>Ca bệnh: ", human_cases, "<br>Mức nguy cơ: ", risk_level),
            hoverinfo = "text",
            marker = list(line = list(width = 0))) %>%
      layout(
        xaxis = list(title = "Số tử vong", showgrid = TRUE, gridcolor = "#f1f5f9", tickfont = list(color = "#64748b", family = "Inter")),
        yaxis = list(title = "", showgrid = FALSE, tickfont = list(color = "#475569", weight = "bold", family = "Inter")),
        bargap = 0.25,
        barmode = "stack"
      ) %>% premium_plotly_layout(lx = 0.15, ly = 1.12)
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

    plot_ly(plot_df, x = ~month_label, y = ~province, z = ~deaths, type = "heatmap",
            colorscale = list(c(0, "#f8fafc"), c(1, "#ef4444")),
            xgap = 3, ygap = 3,
            text = ~hover_text, hoverinfo = "text", showscale = TRUE) %>%
      layout(
        xaxis = list(title = "", tickfont = list(color = "#64748b", family = "Inter")),
        yaxis = list(title = "", tickfont = list(color = "#475569", weight = "bold", family = "Inter"))
      ) %>% premium_plotly_layout(showlegend = FALSE)
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
      hole = 0.65,
      marker = list(colors = risk_colors[as.character(plot_df$risk_level)], line = list(color = '#FFFFFF', width = 2)),
      textinfo = "percent",
      hoverinfo = "label+value+percent"
    ) %>% premium_plotly_layout()
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
        `KTC 95% (thấp)` = round(baseline_ci_lower, 1),
        `KTC 95% (cao)` = round(baseline_ci_upper, 1),
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
          c("#fee2e2", "#fef3c7", "#d1fae5")
        )
      ) %>%
      formatRound(c("Baseline tử vong", "Ngưỡng cảnh báo", "KTC 95% (thấp)", "KTC 95% (cao)", "Tỷ lệ tiêm (%)", "Tỷ lệ dùng huyết thanh (%)", "Tỷ lệ tử vong (%)"), digits = 1)
  })

  output$risk_bar <- renderPlotly({
    plot_df <- province_summary() %>%
      arrange(desc(risk_score), desc(deaths), desc(human_cases)) %>%
      slice_head(n = 15) %>%
      mutate(province = forcats::fct_reorder(province, deaths + human_cases * 0.2))
    
    p <- ggplot(plot_df, aes(x = province, y = deaths)) +
      # Dải khoảng tin cậy 95% - vẽ trước để nằm dưới cột
      geom_ribbon(
        aes(ymin = baseline_ci_lower, ymax = baseline_ci_upper, group = 1),
        fill = "grey70", alpha = 0.3
      ) +
      geom_col(aes(fill = risk_level), width = 0.75) +
      coord_flip() +
      scale_fill_manual(values = risk_colors, drop = FALSE) +
      labs(x = NULL, y = "Số tử vong", fill = "Mức nguy cơ",
           caption = "Vùng xám: khoảng tin cậy 95% kỳ vọng dựa trên lịch sử địa phương") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", panel.grid.minor = element_blank())
    
    ggplotly(p, tooltip = "text")
  })

  # =========================
  # Tab phân tích địa phương
  # =========================
  output$local_bites <- renderInfoBox({
    infoBox(
      "Phơi nhiễm",
      comma(sum(local_data()$animal_bites, na.rm = TRUE)),
      icon = icon("person-circle-exclamation"),
      color = "light-blue")
  })

  output$local_vax <- renderInfoBox({
    rate <- safe_rate(sum(local_data()$vaccinated, na.rm = TRUE), sum(local_data()$animal_bites, na.rm = TRUE))
    infoBox(
      "Tỷ lệ tiêm",
      paste0(rate, "%"),
      icon = icon("syringe"),
      color = "green")
  })

  output$local_serum <- renderInfoBox({
    rate <- safe_rate(sum(local_data()$serum, na.rm = TRUE), sum(local_data()$animal_bites, na.rm = TRUE))
    infoBox(
      "Tỷ lệ huyết thanh",
      paste0(rate, "%"),
      icon = icon("droplet"),
      color = "purple")
  })

  output$local_cases <- renderInfoBox({
    infoBox(
      "Ca bệnh",
      comma(sum(local_data()$human_cases, na.rm = TRUE)),
      icon = icon("hospital"),
      color = "yellow")
  })

  output$local_deaths <- renderInfoBox({
    local_risk <- province_summary() %>% filter(province == local_selected()) %>% pull(risk_level) %>% as.character()
    color <- ifelse(local_risk == "Đỏ", "red", ifelse(local_risk == "Vàng", "yellow", "green"))
    infoBox(
      "Tử vong",
      paste0(comma(sum(local_data()$deaths, na.rm = TRUE)), " - ", local_risk),
      icon = icon("triangle-exclamation"),
      color = color)
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

    plot_ly(plot_df, x = ~month_label) %>%
      add_trace(
        y = ~human_cases, name = "Ca bệnh ở người", 
        type = "scatter", mode = "lines+markers",
        line = list(color = "#f59e0b", width = 3, shape = "spline", smoothing = 1.3),
        marker = list(color = "#f59e0b", size = 8),
        fill = "tozeroy", fillcolor = "rgba(245, 158, 11, 0.1)",
        hovertemplate = "<b>%{x}</b><br>Ca bệnh: %{y}<extra></extra>"
      ) %>%
      add_trace(
        y = ~deaths, name = "Tử vong", 
        type = "scatter", mode = "lines+markers",
        line = list(color = "#ef4444", width = 3, shape = "spline", smoothing = 1.3),
        marker = list(color = "#ef4444", size = 8),
        fill = "tozeroy", fillcolor = "rgba(239, 68, 68, 0.1)",
        hovertemplate = "<b>%{x}</b><br>Tử vong: %{y}<extra></extra>"
      ) %>%
      layout(
        xaxis = list(title = "", showgrid = FALSE, tickfont = list(color = "#64748b", family = "Inter")),
        yaxis = list(title = "Số ca", showgrid = TRUE, gridcolor = "#f1f5f9", zeroline = FALSE, tickfont = list(color = "#64748b", family = "Inter")),
        hovermode = "x unified"
      ) %>% premium_plotly_layout(lx = 0.15, ly = 1.12)
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
        serum_rate = mean(serum_rate, na.rm = TRUE),
        .groups = "drop"
      )

    plot_df <- tibble::tibble(
      `Chỉ số` = c("Ca bệnh", "Tử vong", "Tỷ lệ tiêm (%)", "Tỷ lệ huyết thanh (%)"),
      `Địa phương đang chọn` = c(selected$human_cases, selected$deaths, selected$vax_rate, selected$serum_rate),
      `Trung bình vùng` = c(region_avg$human_cases, region_avg$deaths, region_avg$vax_rate, region_avg$serum_rate),
      panel = c("Số ca", "Số ca", "Tỷ lệ (%)", "Tỷ lệ (%)")
    ) %>%
      pivot_longer(cols = c("Địa phương đang chọn", "Trung bình vùng"), names_to = "Nhóm", values_to = "Giá trị") %>%
      mutate(panel = factor(panel, levels = c("Số ca", "Tỷ lệ (%)")))

    p <- ggplot(plot_df, aes(x = `Chỉ số`, y = `Giá trị`, fill = Nhóm)) +
      geom_col(position = "dodge", width = 0.7) +
      facet_wrap(~panel, scales = "free", ncol = 1) +
      labs(x = NULL, y = NULL, fill = NULL) +
      premium_theme(base_size = 12)

    premium_plotly_layout(ggplotly(p, tooltip = c("x", "y", "fill")), lx = 0.05, ly = 1.06)
  })
  
  output$correlation_plot <- renderPlotly({
    df <- province_summary()
    req(nrow(df) > 0)
    
    # Lọc các tỉnh có dữ liệu hợp lệ
    plot_df <- df %>%
      filter(!is.na(vax_rate) & !is.na(fatality_rate)) %>%
      mutate(
        vax_rate = as.numeric(vax_rate),
        fatality_rate = as.numeric(fatality_rate),
        # Thêm tooltip chi tiết
        tooltip_text = paste0(
          "Tỉnh: ", province, "<br>",
          "Tỷ lệ tiêm: ", vax_rate, "%<br>",
          "Tỷ lệ tử vong/ca bệnh: ", fatality_rate, "%"
        )
      )
    
    # Vẽ biểu đồ scatter với đường xu hướng và dải KTC 95%
    p <- ggplot(plot_df, aes(x = vax_rate, y = fatality_rate)) +
      geom_point(aes(text = tooltip_text, color = risk_level), size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "#2c3e50", fill = "grey70", alpha = 0.3) +
      scale_color_manual(values = risk_colors, drop = FALSE) +
      labs(
        x = "Tỷ lệ tiêm vắc xin (%)",
        y = "Tỷ lệ tử vong/ca bệnh (%)",
        color = "Mức nguy cơ"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.15, y = 1.12))
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

    plot_ly(plot_df, x = ~sex, y = ~n, type = "bar", color = ~sex,
            text = ~paste0("Giới tính: ", sex, "<br>Số tử vong: ", n),
            hoverinfo = "text",
            marker = list(line = list(width = 0))) %>%
      layout(
        xaxis = list(title = "", tickfont = list(color = "#64748b", family = "Inter")),
        yaxis = list(title = "Số tử vong", showgrid = TRUE, gridcolor = "#f1f5f9", tickfont = list(color = "#64748b", family = "Inter")),
        bargap = 0.3
      ) %>% premium_plotly_layout(showlegend = FALSE)
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

    plot_ly(plot_df, y = ~age_group_ordered, x = ~n, type = "bar", orientation = "h",
            marker = list(color = "#f59e0b", line = list(width = 0)),
            text = ~hover_text, hoverinfo = "text") %>%
      layout(
        xaxis = list(title = "Số tử vong", showgrid = TRUE, gridcolor = "#f1f5f9", tickfont = list(color = "#64748b", family = "Inter")),
        yaxis = list(title = "Nhóm tuổi", showgrid = FALSE, tickfont = list(color = "#475569", weight = "bold", family = "Inter")),
        bargap = 0.25
      ) %>% premium_plotly_layout(showlegend = FALSE)
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
  
  # Tạo dữ liệu dự báo theo tỉnh + mùa vụ
  forecast_data <- reactive({
    req(input$forecast_province, main_df())
    
    df_prov <- main_df() %>%
      filter(province == input$forecast_province) %>%
      arrange(year, month)
    
    if (nrow(df_prov) < 6) {
      return(NULL)
    }
    
    all_years <- sort(unique(df_prov$year))
    
    # Cần tối thiểu 2 năm: ít nhất 1 năm để train, 1 năm để test
    if (length(all_years) < 2) {
      return(NULL)
    }
    
    # Năm cuối cùng làm tập kiểm định (test), các năm còn lại làm tập huấn luyện (train)
    test_year <- max(all_years)
    train_years <- all_years[all_years < test_year]
    
    train_df <- df_prov %>% filter(year %in% train_years)
    test_df  <- df_prov %>% filter(year == test_year)
    
    # Baseline tính TỪ TẬP TRAIN, KHÔNG bao gồm năm test
    recent_years <- train_df %>%
      group_by(month) %>%
      summarise(
        mean_deaths = mean(deaths, na.rm = TRUE),
        sd_deaths = ifelse(n() > 1, sd(deaths, na.rm = TRUE), 0),
        mean_cases = mean(human_cases, na.rm = TRUE),
        sd_cases = ifelse(n() > 1, sd(human_cases, na.rm = TRUE), 0),
        n = n(),
        .groups = "drop"
      ) %>%
      mutate(
        sd_deaths = ifelse(is.na(sd_deaths), 0, sd_deaths),
        sd_cases = ifelse(is.na(sd_cases), 0, sd_cases),
        ci_lower_deaths = pmax(0, mean_deaths - 1.96 * (sd_deaths / sqrt(n))),
        ci_upper_deaths = mean_deaths + 1.96 * (sd_deaths / sqrt(n)),
        ci_lower_cases = pmax(0, mean_cases - 1.96 * (sd_cases / sqrt(n))),
        ci_upper_cases = mean_cases + 1.96 * (sd_cases / sqrt(n))
      )
    
    future_months <- data.frame(
      month = 1:12,
      pred_deaths = recent_years$mean_deaths,
      pred_cases = recent_years$mean_cases,
      ci_lower_deaths = recent_years$ci_lower_deaths,
      ci_upper_deaths = recent_years$ci_upper_deaths,
      ci_lower_cases = recent_years$ci_lower_cases,
      ci_upper_cases = recent_years$ci_upper_cases
    )
    
    list(
      actual = train_df,        # Dữ liệu vẽ đường "thực tế" trên biểu đồ — dùng train
      test_actual = test_df,    # Dữ liệu THẬT của năm test — dùng để đánh giá
      forecast = future_months,
      province = input$forecast_province,
      test_year = test_year,
      train_years = train_years
    )
  })
  
  # =========================
  # Tab Dự báo & Đánh giá
  # =========================
  output$forecast_plot <- renderPlotly({
    validate(
      need(forecast_data(), "Không đủ dữ liệu để dự báo cho tỉnh này. (Cần ít nhất 6 tháng số liệu)")
    )
    data <- forecast_data()
    
    # Lấy dữ liệu thực tế (trung bình theo tháng)
    actual_last_2y <- data$actual %>%
      group_by(month) %>%
      summarise(
        deaths = mean(deaths, na.rm = TRUE),
        human_cases = mean(human_cases, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Lấy dự báo
    forecast <- data$forecast
    
    # Tạo dataframe cho biểu đồ
    plot_df <- actual_last_2y %>%
      mutate(
        type = "Thực tế",
        deaths_lower = NA,
        deaths_upper = NA,
        cases_lower = NA,
        cases_upper = NA
      ) %>%
      bind_rows(
        forecast %>%
          mutate(
            type = "Dự báo",
            deaths = pred_deaths,
            human_cases = pred_cases,
            deaths_lower = ci_lower_deaths,
            deaths_upper = ci_upper_deaths,
            cases_lower = ci_lower_cases,
            cases_upper = ci_upper_cases
          )
      )
    
    # Vẽ biểu đồ ggplot
    p <- ggplot(plot_df, aes(x = month)) +
      geom_ribbon(data = plot_df %>% filter(type == "Dự báo"), 
                  aes(ymin = deaths_lower, ymax = deaths_upper, fill = "KTC 95% (Tử vong)"), alpha = 0.25) +
      geom_ribbon(data = plot_df %>% filter(type == "Dự báo"), 
                  aes(ymin = cases_lower, ymax = cases_upper, fill = "KTC 95% (Ca bệnh)"), alpha = 0.25) +
      geom_line(data = plot_df %>% filter(type == "Thực tế"), 
                aes(y = deaths, color = "Tử vong (thực tế)"), linewidth = 1.2) +
      geom_point(data = plot_df %>% filter(type == "Thực tế"), 
                 aes(y = deaths, color = "Tử vong (thực tế)",
                     text = paste0("Tháng: T", month, "<br>Tử vong: ", round(deaths, 1))), 
                 size = 2) +
      geom_line(data = plot_df %>% filter(type == "Thực tế"), 
                aes(y = human_cases, color = "Ca bệnh (thực tế)"), linewidth = 1.2) +
      geom_point(data = plot_df %>% filter(type == "Thực tế"), 
                 aes(y = human_cases, color = "Ca bệnh (thực tế)",
                     text = paste0("Tháng: T", month, "<br>Ca bệnh: ", round(human_cases, 1))), 
                 size = 2) +
      geom_line(data = plot_df %>% filter(type == "Dự báo"), 
                aes(y = deaths, color = "Tử vong (dự báo)"), linetype = "dashed", linewidth = 1.2) +
      geom_point(data = plot_df %>% filter(type == "Dự báo"), 
                 aes(y = deaths, color = "Tử vong (dự báo)",
                     text = paste0("Tháng: T", month, "<br>Tử vong dự báo: ", round(deaths, 1))), 
                 size = 2, alpha = 0) +
      geom_line(data = plot_df %>% filter(type == "Dự báo"), 
                aes(y = human_cases, color = "Ca bệnh (dự báo)"), linetype = "dashed", linewidth = 1.2) +
      geom_point(data = plot_df %>% filter(type == "Dự báo"), 
                 aes(y = human_cases, color = "Ca bệnh (dự báo)",
                     text = paste0("Tháng: T", month, "<br>Ca bệnh dự báo: ", round(human_cases, 1))), 
                 size = 2, alpha = 0) +
      scale_color_manual(
        name = "Chỉ số",
        values = c(
          "Tử vong (thực tế)" = "#e74c3c",
          "Tử vong (dự báo)" = "#e74c3c",
          "Ca bệnh (thực tế)" = "#3498db",
          "Ca bệnh (dự báo)" = "#3498db"
        )
      ) +
      scale_fill_manual(
        name = "Khoảng tin cậy 95%",
        values = c(
          "KTC 95% (Tử vong)" = "rgba(231, 76, 60, 0.2)",
          "KTC 95% (Ca bệnh)" = "rgba(52, 152, 219, 0.2)"
        )
      ) +
      labs(
        title = paste("Dự báo xu hướng ca bệnh và tử vong –", data$province),
        x = "Tháng",
        y = "Số ca"
      ) +
      scale_x_continuous(breaks = 1:12, labels = paste0("T", 1:12)) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12, face = "bold")
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = FALSE, margin = list(b = 60))
  })
  
  output$forecast_legend <- renderUI({
    req(forecast_data())
    HTML("
    <div style='display:flex; flex-wrap:wrap; gap:18px; justify-content:center;
                padding:12px; font-size:13px; background:#f8f9fa; border-radius:8px;'>
      <span><span style='display:inline-block;width:16px;height:3px;
            background:#e74c3c;vertical-align:middle;'></span> Tử vong (thực tế)</span>
      <span><span style='display:inline-block;width:16px;height:0;
            border-top:3px dashed #e74c3c;vertical-align:middle;'></span> Tử vong (dự báo)</span>
      <span><span style='display:inline-block;width:16px;height:3px;
            background:#3498db;vertical-align:middle;'></span> Ca bệnh (thực tế)</span>
      <span><span style='display:inline-block;width:16px;height:0;
            border-top:3px dashed #3498db;vertical-align:middle;'></span> Ca bệnh (dự báo)</span>
      <span><span style='display:inline-block;width:16px;height:12px;
            background:rgba(231,76,60,0.25);vertical-align:middle;'></span> KTC 95% - Tử vong</span>
      <span><span style='display:inline-block;width:16px;height:12px;
            background:rgba(52,152,219,0.25);vertical-align:middle;'></span> KTC 95% - Ca bệnh</span>
    </div>
  ")
  })
  
  # =========================
  # Bảng Đánh giá mô hình dự báo
  # =========================
  output$forecast_metrics <- renderUI({
    req(forecast_data())
    data <- forecast_data()
    
    # Nếu năm test không đủ dữ liệu
    if (nrow(data$test_actual) == 0) {
      return(HTML("<div class='alert alert-warning'>Không đủ dữ liệu năm kiểm định để đánh giá.</div>"))
    }
    
    # Dữ liệu THẬT của năm test (KHÔNG phải trung bình lại)
    test_df <- data$test_actual %>%
      group_by(month) %>%
      summarise(
        actual_deaths = sum(deaths, na.rm = TRUE),
        actual_cases = sum(human_cases, na.rm = TRUE),
        .groups = "drop"
      )
    
    forecast_df <- data$forecast
    
    eval_df <- inner_join(test_df, forecast_df, by = "month")
    
    if (nrow(eval_df) == 0) {
      return(HTML("<div class='alert alert-warning'>Không đủ dữ liệu khớp nhau để đánh giá.</div>"))
    }
    
    # RMSE đúng
    rmse_d <- sqrt(mean((eval_df$actual_deaths - eval_df$pred_deaths)^2, na.rm = TRUE))
    rmse_c <- sqrt(mean((eval_df$actual_cases - eval_df$pred_cases)^2, na.rm = TRUE))
    
    # R² đúng định nghĩa: 1 - SS_residual/SS_total (KHÔNG dùng cor^2)
    calc_r2 <- function(actual, pred) {
      ss_res <- sum((actual - pred)^2, na.rm = TRUE)
      ss_tot <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
      if (ss_tot == 0) return(NA)
      1 - ss_res / ss_tot
    }
    
    r2_d <- calc_r2(eval_df$actual_deaths, eval_df$pred_deaths)
    r2_c <- calc_r2(eval_df$actual_cases, eval_df$pred_cases)
    
    HTML(paste0(
      "<div style='padding: 15px; background: #ffffff; border: 1px solid #ddd; border-radius: 8px;'>",
      "  <h4 style='color: #2c3e50; font-weight: bold;'>📊 Kết quả đánh giá mô hình (Out-of-sample)</h4>",
      "  <p style='color:#7f8c8d; font-size:13px;'>Huấn luyện: ", paste(range(data$train_years), collapse="–"),
      " | Kiểm định: năm ", data$test_year, "</p>",
      "  <table class='table table-bordered'>",
      "    <thead style='background-color: #f8f9fa;'><tr><th>Chỉ số</th><th>Số ca mắc</th><th>Số ca tử vong</th></tr></thead>",
      "    <tbody>",
      "      <tr><td><b>RMSE</b></td><td>", round(rmse_c, 2), "</td><td>", round(rmse_d, 2), "</td></tr>",
      "      <tr><td><b>R²</b></td><td>", round(r2_c, 2), "</td><td>", round(r2_d, 2), "</td></tr>",
      "    </tbody>",
      "  </table>",
      "</div>"
    ))
  })
}

shinyApp(ui, server)
