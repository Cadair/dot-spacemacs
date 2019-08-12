# Spacemacs Config

This is my spacemacs config to use it run the following in your home dir:

    git clone git@github.com:Cadair/dot-spacemacs.git ~/.config/spacemacs
    
    mkdir -p .config/systemd/user
    ln ~/.config/spacemacs/emacsd.service ~/.config/systemd/user/emacsd.service
    systemctl enable --now --user emacsd.service
    
    ln -s ~/.config/spacemacs/dot_spacemacs ~/.spacemacs

## Credit

Much of this is adapted from http://doc.norang.ca/org-mode.html
