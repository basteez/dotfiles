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
    filesystem = {
      filtered_items = {
        visible = false, -- when true, they will just be displayed differently than normal items
        hide_dotfiles = false,
        hide_gitignored = true,
        hide_hidden = true, -- only works on Windows for hidden files/directories
        hide_by_name = {
          ".DS_Store",
          "thumbs.db"
        },
        hide_by_pattern = {
          --"*.meta",
        },
        always_show = { -- remains visible even if other settings would normally hide it
          --".gitignored",
        },
        never_show = { -- remains hidden even if visible is toggled to true
          --".DS_Store",
          --"thumbs.db"
        },
      },
    },
  },
  config = function(_, opts)
    require("neo-tree").setup(opts)
    
    -- Neo-tree keymap
    vim.keymap.set('n', '<leader>n', ':Neotree filesystem toggle left<CR>', { desc = 'Toggle Neo-tree' })
  end,
}
