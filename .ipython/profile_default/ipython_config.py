c = get_config()

c.TerminalIPythonApp.display_banner = False

c.InteractiveShellApp.exec_lines = [
 'from IPython.core.debugger import Pdb',
 'pdb = Pdb()'
]

c.InteractiveShell.autoindent = True
c.InteractiveShell.confirm_exit = False
c.InteractiveShell.deep_reload = True
c.InteractiveShell.xmode = 'Context'

c.TerminalInteractiveShell.autocall = 1

c.PromptManager.justify = True

c.PrefilterManager.multi_line_specials = True

c.AliasManager.user_aliases = [
  ('la', 'ls -al')
]
