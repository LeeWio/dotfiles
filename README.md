# Emacs Configuration

现代化的 Emacs 配置，针对 Java 和 TypeScript/React 开发优化。

## ✨ 特性

- 🚀 快速启动（~0.2秒）
- 🎨 Catppuccin Mocha 主题
- 💡 LSP 支持（Java + TypeScript）
- 🔧 Company 智能补全
- 📦 模块化配置结构
- 🎯 平滑滚动
- 🌈 现代 UI（Doom Modeline）

## 📋 要求

- Emacs 29+
- JDK 17+（Java 开发）
- Node.js + npm（TypeScript 开发）

## 🚀 快速开始

### 1. 克隆配置

```bash
git clone <your-repo> ~/.emacs.d
```

### 2. 安装语言服务器

```bash
# TypeScript/JavaScript
npm install -g typescript-language-server typescript

# Tailwind CSS
npm install -g @tailwindcss/language-server

# Prettier
npm install -g prettier
```

### 3. 首次启动

第一次启动 Emacs 时，会自动：
- 安装所有包
- 下载 Lombok jar
- 下载 Eclipse JDT Language Server（Java）

### 4. 安装图标字体

在 Emacs 中执行：
```
M-x all-the-icons-install-fonts
```

## 📁 配置结构

```
~/.emacs.d/
├── init.el              # 主入口
├── early-init.el        # 启动优化
├── core/                # 核心功能
│   ├── init-vars.el     # 全局变量
│   └── init-benchmark.el # 性能测量
├── modules/             # 功能模块
│   ├── init-ui.el       # UI配置
│   ├── init-modeline.el # Modeline配置
│   ├── init-packages.el # 包管理
│   ├── init-java.el     # Java配置
│   ├── init-typescript.el # TypeScript配置
│   └── init-lang.el     # 其他语言
└── config/              # 本地配置（git忽略）
    └── local-settings.el
```

## 🎮 常用快捷键

### LSP
- `C-c l g g` - 跳转到定义
- `C-c l g r` - 查找引用
- `C-c l r r` - 重命名
- `C-c l a a` - 代码操作

### Git
- `C-x g` - Magit status
- `C-x v t` - Git time machine

### 导航
- `C-x b` - 切换 buffer（consult）
- `C-c f` - 查找文件（consult）
- `C-c g` - Grep（consult）

### 补全
- `TAB` / `RET` - 选择补全
- `C-n` / `C-p` - 上下选择

## 🔧 自定义配置

本地配置放在 `~/.emacs.d/config/local-settings.el`（已在 .gitignore 中）：

```elisp
;; 示例：覆盖配置
(setq company-idle-delay 0.2)
(setq lsp-java-vmargs '("-Xmx4G" ...))
```

## 📚 文档

详细文档请查看 `AGENTS.md`。

## 🛠️ 编译配置（可选）

提升性能：
```
M-x my/compile-config
```
或按 `C-c c c`

## 🐛 常见问题

### LSP 没有自动启动

```
M-x lsp
```

### Lombok 不工作

确保 `~/.emacs.d/lombok.jar` 存在，重启 LSP：
```
M-x lsp-workspace-restart
```

### 补全很慢

调整延迟：
```elisp
(setq company-idle-delay 0.0)  ;; 立即触发
(setq lsp-idle-delay 0.2)      ;; LSP响应时间
```

## 📝 许可

MIT License
