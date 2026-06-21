# Dashboard Giám sát và Cảnh báo Nguy cơ Bệnh Dại tại Việt Nam (2019–2024)

Mã nguồn dashboard giám sát và cảnh báo nguy cơ bệnh dại tại Việt Nam giai đoạn 2019–2024, xây dựng bằng R Shiny nhằm trực quan hóa dữ liệu, theo dõi chỉ số giám sát, phân tầng nguy cơ và dự báo xu hướng theo tỉnh/thành. Sản phẩm thực hiện trong khuôn khổ đồ án tốt nghiệp ngành Khoa học Dữ liệu, Trường Đại học Y tế Công cộng.

> **Lưu ý:** Dữ liệu sử dụng là dữ liệu mô phỏng, phục vụ thiết kế, kiểm thử và minh họa chức năng dashboard. Không phải số liệu giám sát bệnh dại chính thức tại Việt Nam, không dùng để kết luận về tình hình dịch tễ thực tế.

## Mục lục

1. [Giới thiệu](#1-giới-thiệu)
2. [Tính năng chính](#2-tính-năng-chính)
3. [Cấu trúc dữ liệu](#3-cấu-trúc-dữ-liệu)
4. [Cài đặt và chạy dashboard](#4-cài-đặt-và-chạy-dashboard)
5. [Nguồn dữ liệu đầu vào](#5-nguồn-dữ-liệu-đầu-vào)
6. [Quy tắc phân tầng nguy cơ](#6-quy-tắc-phân-tầng-nguy-cơ)
7. [Mô hình dự báo và đánh giá](#7-mô-hình-dự-báo-và-đánh-giá)
8. [Cấu trúc dự án](#8-cấu-trúc-dự-án)
9. [Thư viện sử dụng](#9-thư-viện-sử-dụng)
10. [Tác giả](#10-tác-giả)

---

## 1. Giới thiệu

Bệnh dại là bệnh truyền nhiễm nguy hiểm, tỷ lệ tử vong gần như 100% khi đã phát bệnh. Trong giám sát y tế công cộng, theo dõi các chỉ số phơi nhiễm, tiêm vắc xin, huyết thanh kháng dại, ca bệnh và tử vong theo thời gian, địa phương và vùng có ý nghĩa quan trọng trong nhận diện khu vực cần chú ý.

Dashboard chuyển đổi dữ liệu bệnh dại dạng bảng thành các thành phần trực quan: KPI, biểu đồ xu hướng, bảng cảnh báo, heatmap, bản đồ dịch tễ, phân tích theo địa phương và mô hình dự báo, minh họa khả năng ứng dụng khoa học dữ liệu trong giám sát bệnh truyền nhiễm.

## 2. Tính năng chính

Dashboard gồm **6 tab chức năng**:

| Tab | Thành phần hiển thị | Mục đích |
|---|---|---|
| Tổng quan | KPI, biểu đồ xu hướng, top tỉnh/thành, heatmap, bảng tổng hợp | Theo dõi nhanh tình hình chung theo năm và vùng |
| Cảnh báo nguy cơ | Cơ cấu mức nguy cơ, danh sách địa phương cần chú ý, bảng cảnh báo, xếp hạng nguy cơ | Nhận diện địa phương thuộc mức Đỏ, Vàng, Xanh |
| Bản đồ dịch tễ | Bản đồ Leaflet, marker theo tỉnh/thành, popup thông tin | Quan sát phân bố nguy cơ theo không gian địa lý |
| Phân tích địa phương | KPI địa phương, xu hướng theo tháng, so sánh trung bình vùng, bảng chi tiết | Phân tích sâu một tỉnh/thành cụ thể |
| Chi tiết tử vong | Biểu đồ tử vong theo giới tính, nhóm tuổi, bảng chi tiết | Mô tả đặc điểm tử vong trong dữ liệu mô phỏng |
| **Dự báo & đánh giá mô hình** | Xu hướng dự báo theo tháng kèm khoảng tin cậy 95%, chỉ số RMSE/R² | Ước lượng xu hướng ca bệnh/tử vong và đánh giá độ tin cậy mô hình |

## 3. Cấu trúc dữ liệu

Hai tệp dữ liệu mô phỏng chính:

**`rabies_dashboard_data.csv`** — 1.800 bản ghi (25 tỉnh/thành × 12 tháng × 6 năm, 2019–2024)

| Biến | Kiểu | Ý nghĩa |
|---|---|---|
| year, month | Số nguyên | Năm, tháng ghi nhận |
| province, region | Chuỗi | Tỉnh/thành, vùng địa lý |
| animal_bites | Số nguyên | Số người phơi nhiễm do động vật cắn/cào |
| vaccinated | Số nguyên | Số người tiêm vắc xin phòng dại |
| serum | Số nguyên | Số người sử dụng huyết thanh kháng dại |
| human_cases | Số nguyên | Số ca bệnh dại ở người |
| deaths | Số nguyên | Số ca tử vong do bệnh dại |

**`rabies_death_detail.csv`** — 858 bản ghi, khớp tổng tử vong ở bảng tổng hợp

| Biến | Kiểu | Ý nghĩa |
|---|---|---|
| year, month, province, region | — | Như trên |
| sex | Chuỗi | Giới tính |
| age_group | Chuỗi | Nhóm tuổi |

## 4. Cài đặt và chạy dashboard

**Yêu cầu:** R ≥ 4.0, khuyến nghị dùng RStudio.

```r
install.packages(c(
  "shiny", "shinydashboard", "dplyr", "tidyr",
  "readr", "readxl", "ggplot2", "plotly",
  "DT", "scales", "leaflet", "googlesheets4"
))
```

**Cấu trúc thư mục cần có:**
rabies-dashboard/

├── rabies_dashboard.R

├── rabies_dashboard_data.csv

├── rabies_death_detail.csv

└── README.md
**Chạy bằng RStudio:** mở `rabies_dashboard.R` → bấm **Run App**.

**Chạy bằng R Console:**
```r
shiny::runApp("rabies_dashboard.R")
```

## 5. Nguồn dữ liệu đầu vào

| Nguồn | Cách sử dụng | Ghi chú |
|---|---|---|
| Dữ liệu mặc định | Đọc trực tiếp từ 2 file CSV trong thư mục dự án | Tự đọc lại theo chu kỳ 5 giây |
| Upload CSV/Excel | Tải file từ giao diện dashboard | Hỗ trợ `.csv`, `.xlsx`, `.xls` |
| Google Sheets | Đọc từ link công khai | Tự cập nhật theo chu kỳ 60 giây |

Đối với Google Sheets, cần 2 sheet đúng tên: `tong_hop` (dữ liệu tổng hợp) và `chi_tiet_tu_vong` (dữ liệu chi tiết tử vong).

## 6. Quy tắc phân tầng nguy cơ

Mỗi tỉnh/thành được phân vào mức **Xanh, Vàng hoặc Đỏ** dựa trên số ca bệnh, số tử vong và ngưỡng cảnh báo so với baseline lịch sử.
