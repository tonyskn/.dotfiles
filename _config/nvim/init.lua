-- Leader must be set before plugins
vim.g.mapleader = ","
vim.g.maplocalleader = ","

-- General settings {{{

vim.opt.laststatus = 2
vim.opt.clipboard = "unnamed"
vim.opt.scrolloff = 3
vim.opt.undofile = true
vim.opt.errorbells = false
vim.opt.visualbell = true
vim.opt.showcmd = true
vim.opt.number = true
vim.opt.ruler = true
vim.opt.cursorline = true
vim.opt.wrap = false
vim.opt.expandtab = true
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2
vim.opt.autoindent = true
vim.opt.listchars = { tab = "» ", eol = "¬", extends = "❯", precedes = "❮" }
vim.opt.wildmode = { "list:longest", "list:full" }
vim.opt.wildignore:append("*.o,*.obj,.git,*.rbc,*.class,.svn,vendor/gems/*")
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.colorcolumn = "120"
vim.opt.shortmess:append("A")
vim.opt.autoread = true
vim.opt.breakindent = true
vim.opt.shada = "'1024"
vim.opt.termguicolors = true
vim.opt.background = "dark"
vim.opt.completeopt = { "longest", "menuone", "preview" }
vim.opt.diffopt:append("iwhite")

vim.api.nvim_create_autocmd("FileType", {
  pattern = { "vim", "lua", "zsh", "tmux" },
  callback = function() vim.opt_local.foldmethod = "marker" end,
})

-- }}}

-- Keybindings {{{

local map = vim.keymap.set

-- leader ; for original , behavior
map("n", "<Leader>;", ",")
-- toggle listchars
map("n", "<Leader>s", "<cmd>set list!<CR>", { silent = true })
-- clear search highlight
map("n", "<Leader><space>", "<cmd>noh<CR>", { silent = true })
-- don't move cursor on *
map("n", "*", "*<C-o>")
-- search for visual selection
map("v", "*", "y/<C-r>\"<CR><C-o>")
-- disable K manual
map("n", "K", "<nop>")
-- reselect visual block after indent
map("v", "<", "<gv")
map("v", ">", ">gv")
-- paste: auto-adjust indent and jump to end
map("n", "p", "]p`]")
map("v", "p", "]p`]")
map("v", "y", "y`]")
map("n", "P", "]P")
-- make Y behave like other capitals
map("", "Y", "y$")
-- improve up/down on wrapped lines
map("n", "j", "gj")
map("n", "k", "gk")
-- keep search matches centered
map("n", "n", "nzzzv")
map("n", "N", "Nzzzv")
-- exit terminal mode
map("t", "<Esc>", "<C-\\><C-n>")

-- emacs home/end in insert/command mode
map("i", "<C-a>", "<Esc>I")
map("i", "<C-e>", "<Esc>A")
map("c", "<C-a>", "<Home>")
map("c", "<C-e>", "<End>")
-- space toggles folding
map("n", "<Space>", "za")
map("v", "<Space>", "za")
-- quick fold navigation
map("n", "zJ", "zjzmza")
map("n", "zK", "zkzmza[z")
-- window shortcuts
map("n", "=", "<cmd>res<CR><cmd>vertical res<CR>", { silent = true })
map("n", "<Leader>=", "<C-w>=")
map("n", "<Leader>l", "<C-w>L")
map("n", "<Leader>p", "<C-w>J")
-- close buffer and go up
map("", "<Leader>q", "<cmd>x<CR><C-w>j", { silent = true })
-- walk through files and diff against master
map("n", "<Leader>id", "<C-w>f:resize<CR><C-j><C-j>:Gvdiff master<CR>", { silent = true })
map("n", "<Leader>if", ":bd<CR>:bd<CR>j<Leader>id", { silent = true })
map("n", "<Leader>iF", ":bd<CR>:bd<CR>dd<Leader>id", { silent = true })
-- sudo write
map("c", "w!!", "w !sudo tee % > /dev/null <CR>")
-- replace visual selection with output of command
map("v", "<Leader>!", function()
  local cmd = vim.fn.input(":")
  if cmd == "" then return end
  local output = vim.fn.system("zsh -lc '" .. cmd .. "'"):gsub("\n$", "")
  vim.fn.setreg("z", output)
  vim.cmd('normal! gv"_d')
  vim.cmd('normal! "zP')
end)
-- format text into columns
map("n", "<Leader>t", ":%!column -t<CR>", { silent = true })
-- diff registers a and b
map("n", "<Leader>V", "<cmd>tabnew<CR><cmd>put a<CR><cmd>diffthis<CR><cmd>vnew<CR><cmd>put b<CR><cmd>diffthis<CR>", { silent = true })
map("n", "<Leader>Q", "<cmd>windo bd!<CR>tabclose<CR>", { silent = true })
-- comment with ,c (uses built-in gc)
map("", ",c", "gc", { remap = true })
-- plugin status
map("n", "<Leader>S", "<cmd>Lazy<CR>", { silent = true })

-- }}}

-- Plugin settings (must be set before lazy.setup) {{{

-- targets.vim
vim.g.targets_pairs = "()b {}c []B"
vim.g.targets_argOpening = "[({[]"
vim.g.targets_argClosing = "[]})]]"

-- fugitive
vim.g.github_enterprise_urls = { "https://ghe.spotify.net" }

-- REST console
vim.g.vrc_set_default_mapping = 0
vim.g.vrc_allow_get_request_body = 1
vim.g.vrc_elasticsearch_support = 1
vim.b.vrc_response_default_content_type = "application/json"
vim.g.vrc_output_buffer_name = "__REST.json"
vim.g.vrc_curl_opts = {
  ["-b"] = vim.fn.expand("$HOME") .. "/.vim/backup/vrc_cookie_jar",
  ["-c"] = vim.fn.expand("$HOME") .. "/.vim/backup/vrc_cookie_jar",
  ["-s"] = "",
  ["-L"] = "",
  ["-k"] = "",
}

-- }}}

-- Bootstrap lazy.nvim {{{

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.uv.fs_stat(lazypath) then
  vim.fn.system({
    "git", "clone", "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- }}}

-- Plugins {{{

require("lazy").setup({

  -- Colorscheme
  {
    "gbprod/nord.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      vim.cmd("colorscheme nord")
    end,
  },

  -- Statusline
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {
      options = {
        theme = "nord",
        icons_enabled = true,
      },
    },
  },

  -- Treesitter (parsers install via :TSInstall)
  {
    "nvim-treesitter/nvim-treesitter",
    lazy = false,
    build = ":TSUpdate",
  },

  -- File tree
  {
    "nvim-tree/nvim-tree.lua",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {
      update_focused_file = { enable = true, update_root = true },
      view = { width = 40 },
      filters = { dotfiles = false },
      renderer = {
        icons = { glyphs = { folder = { arrow_closed = "+", arrow_open = "-" } } },
      },
      actions = { open_file = { quit_on_open = false } },
    },
    keys = {
      { "<Leader>nd", "<cmd>NvimTreeToggle<CR>", desc = "Toggle file tree" },
      { "<Leader>nf", "<cmd>NvimTreeFindFile<CR>", desc = "Reveal file in tree" },
    },
  },

  -- Telescope (fuzzy finder, ivy theme = bottom panel like fzf)
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
    },
    cmd = "Telescope",
    keys = {
      { "<Leader>nt", function() require("telescope.builtin").find_files({ hidden = true }) end, desc = "Find files" },
      { "<Leader>nb", function() require("telescope.builtin").buffers() end, desc = "Buffers" },
      { "<Leader>ne", function() require("telescope.builtin").oldfiles() end, desc = "Recent files" },
      { "<Leader>nr", function() require("telescope.builtin").command_history() end, desc = "Command history" },
      { "<Leader>nc", function() require("telescope.builtin").git_bcommits() end, desc = "Buffer commits" },
      { "<Leader>nc", function() require("telescope.builtin").git_bcommits_range() end, mode = "v", desc = "Range commits" },
      { "<Leader>ns", function() require("telescope.builtin").git_status() end, desc = "Git status" },
      { "<Leader>ng", function() require("telescope.builtin").live_grep() end, desc = "Live grep" },
      { "<Leader>G", function() require("telescope.builtin").grep_string() end, desc = "Grep word under cursor" },
    },
    config = function()
      local telescope = require("telescope")
      local actions = require("telescope.actions")
      telescope.setup({
        defaults = require("telescope.themes").get_ivy({
          layout_config = { height = 0.3 },
          mappings = {
            i = {
              ["<C-x>"] = actions.select_horizontal,
              ["<C-v>"] = actions.select_vertical,
            },
          },
        }),
      })
      telescope.load_extension("fzf")
    end,
  },

  -- Git
  { "tpope/vim-fugitive" },
  { "tpope/vim-rhubarb" },
  {
    "lewis6991/gitsigns.nvim",
    event = "VeryLazy",
    opts = {
      on_attach = function(bufnr)
        local gs = require("gitsigns")
        local opts = function(desc) return { buffer = bufnr, desc = desc } end
        map("n", "<Leader>d", function()
          if vim.wo.diff then vim.cmd("normal! ]czz") else gs.next_hunk() end
        end, opts("Next diff/hunk"))
        map("n", "<Leader>D", function()
          if vim.wo.diff then vim.cmd("normal! [czz") else gs.prev_hunk() end
        end, opts("Prev diff/hunk"))
        map("n", "<Leader>gs", gs.stage_hunk, opts("Stage hunk"))
        map("n", "<Leader>gr", gs.reset_hunk, opts("Reset hunk"))
        map("v", "<Leader>gs", function() gs.stage_hunk({ vim.fn.line("v"), vim.fn.line(".") }) end, opts("Stage selection"))
        map("v", "<Leader>gr", function() gs.reset_hunk({ vim.fn.line("v"), vim.fn.line(".") }) end, opts("Reset selection"))
      end,
    },
  },

  -- Text objects and editing
  { "wellle/targets.vim" },
  {
    "kylechui/nvim-surround",
    event = "VeryLazy",
    opts = {},
  },

  -- Motion (leap = successor to vim-sneak)
  {
    url = "https://codeberg.org/andyg/leap.nvim",
    config = function()
      map({ "n", "x", "o" }, "s", "<Plug>(leap-forward)")
      map({ "n", "x", "o" }, "S", "<Plug>(leap-backward)")
    end,
  },

  -- REST client
  { "diepm/vim-rest-console", ft = "rest" },
})

-- }}}

-- REST console autocmds {{{

vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
  pattern = "*.rest",
  callback = function()
    map("n", "<C-i>", ":call VrcQuery()<CR>", { buffer = true, silent = true })
  end,
})
vim.api.nvim_create_autocmd("BufWinLeave", { pattern = "*.rest", command = "mkview" })
vim.api.nvim_create_autocmd("BufWinEnter", { pattern = "*.rest", command = "silent! loadview" })
vim.api.nvim_create_autocmd("BufEnter", { pattern = "__REST.json", command = "set modifiable" })

-- }}}
