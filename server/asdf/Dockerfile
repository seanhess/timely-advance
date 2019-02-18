FROM timely:latest


RUN apt-get install cron
ADD crontab /etc/cron.d/test-cron

# Add shell script and grant execution rights
# ADD script.sh /etc/cron.hourly/script.sh
# RUN chmod +x /etc/cron.hourly/script.sh

# # Give execution rights on the cron job
RUN chmod 0644 /etc/cron.d/test-cron

# Create the log file to be able to run tail
RUN echo "STARTED" >> /var/log/cron.log 2>&1

# Run the command on container startup
CMD cron && tail -f /var/log/cron.log
