vim.api.nvim_create_user_command("FormatErlang", function(args)
    local erl = require("d464.erlang")
    erl.format_buffer(0)
end, { force = true })
