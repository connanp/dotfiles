let g:dotvim_settings = {}
let g:dotvim_settings.version = 1

let g:dotvim_settings.colorscheme = 'ir_black'

let g:dotvim_settings.plugin_groups_include = ['web', 'python', 'javascript']
let g:dotvim_settings.plugin_groups_exclude = []

" Put all site-local dotvim_settings in the below file
" to add to plugin groups, do 
" call add(g:dotvim_settings.plugin_groups_include, 'blah')
if filereadable(expand('~/.vim/vimrc.local'))
  execute 'source ~/.vim/vimrc.local'
endif

source ~/.vim/vimrc

" All other site-local vim settings
if filereadable(expand('~/.vim/vimrc.after'))
  execute 'source ~/.vim/vimrc.after'
endif
