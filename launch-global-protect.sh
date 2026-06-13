#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Launch Global Protect
# @raycast.mode compact

# Optional parameters:
# @raycast.icon 🤖

# Documentation:
# @raycast.author Aki Suzuki
# @raycast.authorURL github.com/suzuki11109

launchctl load /Library/LaunchAgents/com.paloaltonetworks.gp.pangp*
