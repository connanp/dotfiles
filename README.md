# Connan’s dotfiles
Loosely based off Mathias’s dotfiles

## Installation

### Using Git and the bootstrap script

```bash
git clone https://github.com/connanp/dotfiles.git && cd dotfiles && source bootstrap.sh
```

To update, `cd` into your local `dotfiles` repository and then:

```bash
source bootstrap.sh
```

Alternatively, to update while avoiding the confirmation prompt:

```bash
set -- -f; source bootstrap.sh
```

### Git-free install

To install these dotfiles without Git:

```bash
cd; curl -#L https://github.com/connanp/dotfiles/tarball/master | tar -xzv --strip-components 1 --exclude={README.md,bootstrap.sh}
```

### Extras

```bash
curl http://beyondgrep.com/ack-2.12-single-file > ~/bin/ack && chmod 0755 !#:3
```
