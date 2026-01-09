# GitHub仓库设置说明

本地Git仓库已经初始化并准备好推送。请按照以下步骤在GitHub上创建仓库并上传代码：

## 步骤 1: 在GitHub上创建仓库

1. 打开浏览器，访问: https://github.com/new
2. 登录您的GitHub账户 (JivonKiang)
3. 填写仓库信息：
   - **Repository name**: `CKM_42`
   - **Description**: (可选) Cardiovascular-Kidney-Metabolic Syndrome Analysis Project
   - **Visibility**: 选择 Public 或 Private
   - **重要**: 不要勾选 "Add a README file"、"Add .gitignore" 或 "Choose a license"
4. 点击 "Create repository" 按钮

## 步骤 2: 推送代码到GitHub

在PowerShell中运行以下命令（仓库地址已配置）：

```powershell
cd "E:\20250110 Cardiovascular-Kidney-Metabolic Syndrome, CKM\20260109 Diabetology metabolic syndrome Revised\Supplementary\Code"
git push -u origin main
```

如果提示输入用户名和密码，请使用：
- 用户名: 您的GitHub用户名
- 密码: 您的GitHub Personal Access Token (不是账户密码)

如果还没有Personal Access Token，请：
1. 访问: https://github.com/settings/tokens
2. 点击 "Generate new token" -> "Generate new token (classic)"
3. 选择权限: 至少勾选 `repo` 权限
4. 生成后复制token（只显示一次）
5. 在git push时使用这个token作为密码

## 验证

推送成功后，访问 https://github.com/JivonKiang/CKM_42 查看您的代码。

## 已上传的内容

- ✅ `Raw Data Processing/` 文件夹（所有.R脚本文件）
- ✅ `Statistics and Visualization/` 文件夹（所有.R脚本文件）
- ✅ `README.md`
- ✅ `.gitignore`

注意：大型数据文件（.RDS, .RData, .xlsx, .csv等）已被`.gitignore`排除，不会上传到GitHub。
