-- Neo-tree file explorer
return {
  "nvim-neo-tree/neo-tree.nvim",
  branch = "v3.x",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-tree/nvim-web-devicons",
    "MunifTanjim/nui.nvim"
  },
  lazy = false,
  opts = {
    -- fill any relevant options here
  },
  config = function(_, opts)
    require("neo-tree").setup(opts)
    
    -- Neo-tree keymap
    vim.keymap.set('n', '<leader>n', ':Neotree filesystem toggle left<CR>', { desc = 'Toggle Neo-tree' })
  end,
}
