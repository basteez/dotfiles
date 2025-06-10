return {
  {
    "mason-org/mason.nvim",
    config = function()
      require("mason").setup({
      })
    end
  },
  {
    "mason-org/mason-lspconfig.nvim",
    config = function()
      require("mason-lspconfig").setup({
        ensure_installed = { 
          "docker_compose_language_service",
          "dockerls",
          "lua_ls",
          "eslint",
          "gdscript",
          --"gdshader_lsp",
          --"golangci_lint_ls",
          "html",
          "java_language_server",
          "jsonls",
          "pico8_ls",
          "ts_ls"
        }
      })
    end
  },
  {
    "neovim/nvim-lspconfig",
    config = function()
      local lspconfig = require("lspconfig")
      lspconfig.docker_compose_language_service.setup({})
      lspconfig.dockerls.setup({})
      lspconfig.lua_ls.setup({})
      lspconfig.eslint.setup({})
      lspconfig.gdscript.setup({})
      --lspconfig.gdshader_lsp.setup({})
      lspconfig.html.setup({})
      lspconfig.java_language_server.setup({})
      lspconfig.jsonls.setup({})
      lspconfig.pico8_ls.setup({})
      lspconfig.ts_ls.setup({})
      vim.keymap.set('n', 'K', vim.lsp.buf.hover, {})
    end
  }
}
