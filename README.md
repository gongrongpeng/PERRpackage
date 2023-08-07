# PERRpackage
PERRpackage
PERRpackage是一个R包，用于使用线性回归、Logistic回归和Cox比例风险回归执行PERR方法。

# 安装
您可以直接从GitHub安装此包：

# 如果还没有安装devtools，请先安装
install.packages("devtools")

# 从GitHub安装PERRpackage
devtools::install_github("gongrongpeng/PERRpackage")
# 使用
# 加载包：
library(PERRpackage)
# 示例
以下是如何使用perr_linear函数的示例：
data <- data.frame(treatment = c(0,1,0,1), outcome = c(5.2,3.4,2.1,4.5))
result <- perr_linear(data, "treatment", "outcome")
# 功能
perr_linear: 使用线性回归执行PERR方法。
perr_logistic: 使用Logistic回归执行PERR方法。
perr_cox: 使用Cox比例风险回归执行PERR方法。
# 作者
Rongpeng Gong(ORICD:0000-0003-1117-0956)

# 许可证
此项目根据GPL-3许可证获得许可。


# PERRpackage
PERRpackage is an R package designed to implement the PERR method using linear regression, logistic regression, and Cox proportional hazards regression.

# Installation
You can install this package directly from GitHub:
# If you haven't installed devtools yet, do so first
install.packages("devtools")

# Install PERRpackage from GitHub
devtools::install_github("gongrongpeng/PERRpackage")
# Usage
# Load the package:
library(PERRpackage)
# Example
# Here's an example of how to use the perr_linear function:
data <- data.frame(treatment = c(0,1,0,1), outcome = c(5.2,3.4,2.1,4.5))
result <- perr_linear(data, "treatment", "outcome")
# Features
perr_linear: Implements the PERR method using linear regression.
perr_logistic: Implements the PERR method using logistic regression.
perr_cox: Implements the PERR method using Cox proportional hazards regression.
# Author
Rongpeng Gong (ORCID:0000-0003-1117-0956)

# License
This project is licensed under the GPL-3 License.

