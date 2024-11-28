
if (!require(httr)) install.packages("httr")
if (!require(jsonlite)) install.packages("jsonlite")
if (!require(plumber)) install.packages("plumber")
if (!require(ltm)) install.packages("ltm")

library(plumber)
library(ltm)
library(httr)
library(jsonlite)

#* @get /hello
function() {
  list(message = "Hello")
}
#* @get /calculate
function() {
  # Tính toán 1 + 1
  result <- 1 + 1
  
  # Hiển thị kết quả
  return(paste("Kết quả của phép toán 1 + 1 là:", result))
}
#* @post /calculateIRT
function(req, res) {
  
  # Kiểm tra kiểu dữ liệu của req$postBody (nếu là raw vector thì dùng rawToChar)
  if (inherits(req$postBody, "raw")) {
    data <- fromJSON(rawToChar(req$postBody))  # Chuyển đổi raw thành chuỗi và sau đó thành JSON
  } else {
    data <- fromJSON(req$postBody)  # Nếu đã là chuỗi JSON, chuyển trực tiếp
  }
  
  # Tính toán mô hình Rasch với dữ liệu
  rasch_model <- rasch(data)
  
  # Trích xuất các kết quả từ mô hình Rasch
  coefficients <- rasch_model$coefficients
  
  # Trả về kết quả dưới dạng JSON
  result <- list(
    coefficients = coefficients[, "beta.i"],
    log_likelihood = rasch_model$log.lik,
    AIC = rasch_model$AIC,
    BIC = rasch_model$BIC
  )
  
  # Gửi kết quả về cho client dưới dạng JSON
  res$setHeader("Content-Type", "application/json")
  return(toJSON(result, auto_unbox = TRUE))
}

