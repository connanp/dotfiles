# Connan’s dotfiles
Loosely based off Mathias’s dotfiles

## Installation

### Using Git and the bootstrap script

```bash
git clone https://github.com/connanp/dotfiles.git .dotfiles && cd .dotfiles && bash bootstrap.sh
```

To update, `cd` into your local `dotfiles` repository and then:

```bash
bash bootstrap.sh
```

Alternatively, to update while avoiding the confirmation prompt:

```bash
set -- -f; bash bootstrap.sh
```

### Git-free install

To install these dotfiles without Git:

```bash
cd; curl -#L https://github.com/connanp/dotfiles/tarball/master | tar -xzv --strip-components 1 --exclude={README.md,bootstrap.sh}
```

### Extras

