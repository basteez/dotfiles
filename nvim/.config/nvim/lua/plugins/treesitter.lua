-- Treesitter syntax highlighting
return {
  "nvim-treesitter/nvim-treesitter",
  branch = 'master',
  lazy = false,
  build = ":TSUpdate",
  config = function()
    require'nvim-treesitter.configs'.setup {
      ensure_installed = { 
        "c", "lua", "vim", "vimdoc", "query", "markdown", "markdown_inline", 
        "java", "html", "javascript", "typescript", "css" 
      },
      sync_install = false,
      auto_install = true,
      ignore_install = { },
      highlight = {
        enable = true,
        additional_vim_regex_highlighting = false,
      },
    }
  end,
}
