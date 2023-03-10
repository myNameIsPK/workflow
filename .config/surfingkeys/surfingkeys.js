// from: https://github.com/brookhong/Surfingkeys/wiki/Migrate-your-settings-from-0.9.74-to-1.0
const {
    aceVimMap,
    mapkey,
    imap,
    imapkey,
    getClickableElements,
    vmapkey,
    map,
    unmap,
    vunmap,
    cmap,
    addSearchAlias,
    removeSearchAlias,
    tabOpenLink,
    readText,
    Clipboard,
    Front,
    Hints,
    Visual,
    RUNTIME
} = api;

// FIX: seem not work
// an example to create a new mapping `ctrl-y`
mapkey('<Ctrl-y>', 'Show me the money', function() {
    Front.showPopup('a well-known phrase uttered by characters in the 1996 film Jerry Maguire (Escape to close).');
});

unmap('<Ctrl-i>');

map('gt', 'T');

map('J', 'R')
map('K', 'E')
map('H', 'S')
map('L', 'D')
unmap('E')
unmap('R')
unmap('S')
unmap('D')

map('<Ctrl-u>', 'e')
map('<Ctrl-d>', 'd')
map('<Ctrl-b>', 'U')
map('<Ctrl-f>', 'P')
unmap('e')
// unmap('d')
unmap('U')
unmap('P')

cmap('<Ctrl-n', '<Tab>')
cmap('<Ctrl-p', '<Shift-Tab>')

imap('<Ctrl-a>', '<Ctrl-f>')

Hints.setCharacters("arstgqwjbxcdv");

api.vmap('n', 'zzn')
api.vmap('N', 'zzN')

// removeSearchAlias('b'); //baidu
// removeSearchAlias('e'); //wikipedia
// removeSearchAlias('w'); //bing

// FIX: not work
// settings.useNeovim = true

// gruvbox dark
settings.theme = `
.sk_theme {
    font-family: Input Sans Condensed, Charcoal, sans-serif;
    font-size: 10pt;
    background: #282828;
    color: #ebdbb2;
}
.sk_theme tbody {
    color: #b8bb26;
}
.sk_theme input {
    color: #d9dce0;
}
.sk_theme .url {
    color: #98971a;
}
.sk_theme .annotation {
    color: #b16286;
}
.sk_theme .omnibar_highlight {
    color: #ebdbb2;
}
.sk_theme #sk_omnibarSearchResult ul li:nth-child(odd) {
    background: #282828;
}
.sk_theme #sk_omnibarSearchResult ul li.focused {
    background: #d3869b;
}
#sk_status, #sk_find {
    font-size: 10pt;
}`;
