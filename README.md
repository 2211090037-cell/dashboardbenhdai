# dashboardbenhdai
Dashboard Giám sát và Cảnh báo Nguy cơ Bệnh Dại tại Việt Nam (2019–2024)
Đây là mã nguồn dashboard giám sát và cảnh báo nguy cơ bệnh dại tại Việt Nam giai đoạn 2019–2024. Dashboard được xây dựng bằng R Shiny nhằm hỗ trợ trực quan hóa dữ liệu, theo dõi các chỉ số giám sát và phân tầng nguy cơ theo tỉnh/thành.
Sản phẩm được thực hiện trong khuôn khổ đồ án tốt nghiệp ngành Khoa học Dữ liệu, Trường Đại học Y tế Công cộng.
> **Lưu ý:** Dữ liệu sử dụng trong dự án là **dữ liệu mô phỏng**, được xây dựng nhằm phục vụ thiết kế, kiểm thử và minh họa chức năng dashboard. Dữ liệu không phải số liệu giám sát bệnh dại chính thức tại Việt Nam và không được sử dụng để kết luận về tình hình dịch tễ thực tế tại bất kỳ địa phương nào.
Mục lục
1. Giới thiệu
2. Tính năng chính
3. Cấu trúc dữ liệu
4. Cài đặt và chạy dashboard
5. Nguồn dữ liệu đầu vào
6. Quy tắc phân tầng nguy cơ
7. Cấu trúc dự án
8. Thư viện sử dụng
9. Tác giả
1. Giới thiệu
Bệnh dại là bệnh truyền nhiễm nguy hiểm, có tỷ lệ tử vong gần như 100% khi đã phát bệnh. Trong giám sát y tế công cộng, việc theo dõi các chỉ số như phơi nhiễm, tiêm vắc xin, sử dụng huyết thanh kháng dại, ca bệnh và tử vong theo thời gian, địa phương và vùng địa lý có ý nghĩa quan trọng trong nhận diện khu vực cần chú ý.
Dashboard này được xây dựng nhằm chuyển đổi dữ liệu bệnh dại dạng bảng thành các thành phần trực quan, bao gồm KPI, biểu đồ xu hướng, bảng cảnh báo, heatmap, bản đồ dịch tễ và phân tích chi tiết theo địa phương. Qua đó, sản phẩm minh họa khả năng ứng dụng khoa học dữ liệu trong giám sát và cảnh báo nguy cơ bệnh truyền nhiễm.
2. Tính năng chính
Dashboard gồm 5 tab chức năng chính:
Tab	Thành phần hiển thị	Mục đích
Tổng quan	KPI, biểu đồ xu hướng, top tỉnh/thành, heatmap, bảng tổng hợp	Theo dõi nhanh tình hình chung theo năm và vùng
Cảnh báo nguy cơ	Cơ cấu mức nguy cơ, danh sách địa phương cần chú ý, bảng cảnh báo, xếp hạng nguy cơ	Nhận diện địa phương thuộc mức Đỏ, Vàng, Xanh
Bản đồ dịch tễ	Bản đồ Leaflet, marker theo tỉnh/thành, popup thông tin	Quan sát phân bố nguy cơ theo không gian địa lý
Phân tích địa phương	KPI địa phương, xu hướng theo tháng, so sánh với trung bình vùng, bảng chi tiết	Phân tích sâu một tỉnh/thành cụ thể
Chi tiết tử vong	Biểu đồ tử vong theo giới tính, nhóm tuổi và bảng chi tiết	Mô tả đặc điểm tử vong trong dữ liệu mô phỏng
3. Cấu trúc dữ liệu
Dự án sử dụng hai tệp dữ liệu mô phỏng chính.
3.1. Bảng dữ liệu tổng hợp
Tệp dữ liệu: `rabies_dashboard_data.csv`
Tên biến	Kiểu dữ liệu	Ý nghĩa
`year`	Số nguyên	Năm ghi nhận dữ liệu
`month`	Số nguyên	Tháng ghi nhận dữ liệu
`province`	Chuỗi ký tự	Tỉnh/thành ghi nhận dữ liệu
`region`	Chuỗi ký tự	Vùng địa lý
`animal_bites`	Số nguyên	Số người phơi nhiễm do động vật cắn/cào
`vaccinated`	Số nguyên	Số người tiêm vắc xin phòng dại
`serum`	Số nguyên	Số người sử dụng huyết thanh kháng dại
`human_cases`	Số nguyên	Số ca bệnh dại ở người
`deaths`	Số nguyên	Số ca tử vong do bệnh dại
Quy mô dữ liệu: 1.800 bản ghi, tương ứng 25 tỉnh/thành, 12 tháng và 6 năm trong giai đoạn 2019–2024.
3.2. Bảng dữ liệu chi tiết tử vong
Tệp dữ liệu: `rabies_death_detail.csv`
Tên biến	Kiểu dữ liệu	Ý nghĩa
`year`	Số nguyên	Năm ghi nhận tử vong
`month`	Số nguyên	Tháng ghi nhận tử vong
`province`	Chuỗi ký tự	Tỉnh/thành ghi nhận tử vong
`region`	Chuỗi ký tự	Vùng địa lý
`sex`	Chuỗi ký tự	Giới tính
`age_group`	Chuỗi ký tự	Nhóm tuổi
Quy mô dữ liệu: 858 bản ghi, khớp với tổng số tử vong trong bảng dữ liệu tổng hợp.
4. Cài đặt và chạy dashboard
4.1. Yêu cầu môi trường
Cần cài đặt:
R phiên bản 4.0 trở lên;
RStudio, khuyến nghị sử dụng để chạy ứng dụng Shiny.
4.2. Cài đặt thư viện R
Chạy lệnh sau trong R Console để cài đặt các thư viện cần thiết:
```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "dplyr",
  "tidyr",
  "readr",
  "readxl",
  "ggplot2",
  "plotly",
  "DT",
  "scales",
  "leaflet",
  "googlesheets4"
))
```
4.3. Chuẩn bị tệp dữ liệu
Đảm bảo thư mục dự án có đủ các tệp sau:
```text
rabies-dashboard/
├── rabies_dashboard.R
├── rabies_dashboard_data.csv
├── rabies_death_detail.csv
└── README.md
```
Trong đó:
`rabies_dashboard.R` là tệp mã nguồn chính của dashboard;
`rabies_dashboard_data.csv` là tệp dữ liệu tổng hợp bắt buộc;
`rabies_death_detail.csv` là tệp dữ liệu chi tiết tử vong, dùng cho phân tích theo giới tính, nhóm tuổi và địa phương.
4.4. Chạy bằng RStudio
Mở thư mục dự án bằng RStudio.
Mở file `rabies_dashboard.R`.
Kiểm tra hai file dữ liệu `.csv` nằm cùng thư mục với file mã nguồn.
Bấm Run App trong RStudio.
Dashboard sẽ mở trong Viewer của RStudio hoặc trình duyệt web.
4.5. Chạy bằng R Console
Tại thư mục chứa file mã nguồn, chạy lệnh:
```r
shiny::runApp("rabies_dashboard.R")
```
5. Nguồn dữ liệu đầu vào
Dashboard hỗ trợ ba hình thức nạp dữ liệu:
Nguồn dữ liệu	Cách sử dụng	Ghi chú
Dữ liệu mặc định	Đọc trực tiếp từ hai file CSV trong thư mục dự án	Tự đọc lại dữ liệu theo chu kỳ 5 giây
Upload CSV/Excel	Người dùng tải file lên từ giao diện dashboard	Hỗ trợ định dạng `.csv`, `.xlsx`, `.xls`
Google Sheets	Đọc dữ liệu từ link Google Sheets công khai	Tự cập nhật theo chu kỳ 60 giây
Đối với Google Sheets, bảng tính cần được chia sẻ ở chế độ công khai và có hai sheet đúng tên:
Tên sheet	Nội dung
`tong_hop`	Dữ liệu tổng hợp bệnh dại
`chi_tiet_tu_vong`	Dữ liệu chi tiết tử vong
6. Quy tắc phân tầng nguy cơ
Dashboard tự động phân loại mỗi tỉnh/thành vào một trong ba mức nguy cơ: Xanh, Vàng hoặc Đỏ. Việc phân loại dựa trên số ca bệnh ở người, số tử vong và ngưỡng cảnh báo so với baseline lịch sử.
6.1. Công thức tính ngưỡng cảnh báo
```text
Baseline = Trung bình số tử vong hằng năm của các năm trước
SD = Độ lệch chuẩn của số tử vong hằng năm trong các năm trước
Ngưỡng cảnh báo = Baseline + 2 × SD
```
6.2. Điều kiện phân loại nguy cơ
Mức nguy cơ	Điều kiện phân loại	Ý nghĩa
Đỏ	`deaths >= 10` hoặc `human_cases >= 20` hoặc vượt ngưỡng cảnh báo lịch sử	Địa phương nguy cơ cao, cần ưu tiên theo dõi
Vàng	`deaths >= 5` hoặc `human_cases >= 10`	Địa phương cần theo dõi
Xanh	Các trường hợp còn lại	Địa phương có nguy cơ thấp hơn trong phạm vi dữ liệu
Với năm đầu tiên trong dữ liệu, do chưa có dữ liệu lịch sử để tính baseline, hệ thống chỉ áp dụng các ngưỡng tuyệt đối về số ca bệnh và số tử vong.
7. Cấu trúc dự án
Tệp	Vai trò
`rabies_dashboard.R`	Chứa toàn bộ mã nguồn dashboard R Shiny, bao gồm UI, Server, xử lý dữ liệu, phân tầng nguy cơ và trực quan hóa
`rabies_dashboard_data.csv`	Dữ liệu tổng hợp mô phỏng, dùng để tính KPI, tỷ lệ, bảng cảnh báo, biểu đồ và bản đồ dịch tễ
`rabies_death_detail.csv`	Dữ liệu chi tiết tử vong mô phỏng, dùng để phân tích theo giới tính, nhóm tuổi và địa phương
`README.md`	Tài liệu hướng dẫn sử dụng mã nguồn và mô tả cấu trúc dự án
8. Thư viện sử dụng
Thư viện	Vai trò
`shiny`	Xây dựng ứng dụng web tương tác
`shinydashboard`	Tạo bố cục dashboard, sidebar, tab và box
`dplyr`	Lọc, nhóm, tổng hợp và biến đổi dữ liệu
`tidyr`	Chuyển đổi cấu trúc dữ liệu phục vụ trực quan hóa
`readr`	Đọc dữ liệu định dạng CSV
`readxl`	Đọc dữ liệu định dạng Excel
`ggplot2`	Xây dựng biểu đồ
`plotly`	Chuyển biểu đồ sang dạng tương tác
`DT`	Hiển thị bảng dữ liệu tương tác
`scales`	Định dạng số liệu và tỷ lệ
`leaflet`	Xây dựng bản đồ dịch tễ tương tác
`googlesheets4`	Đọc dữ liệu từ Google Sheets
9. Tác giả
Nguyễn Thị Hải Yến  
MSSV: 2211090037  
Đồ án tốt nghiệp Cử nhân chính quy ngành Khoa học Dữ liệu  
Trường Đại học Y tế Công cộng — Hà Nội, 2026  
Giảng viên hướng dẫn: PGS.TS Phạm Việt Cường
---
Dữ liệu sử dụng trong dự án là dữ liệu mô phỏng, phục vụ mục đích học thuật, thiết kế và kiểm thử dashboard.
