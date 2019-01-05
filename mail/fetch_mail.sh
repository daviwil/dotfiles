# This is meant to be run by cron
# Run `crontab -e` and add the following line:
# */3 * * * * /home/user/.dotfiles/mail/fetch_mail.sh

killall offlineimap
offlineimap -u quiet
