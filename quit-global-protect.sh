#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Quit Global Protect
# @raycast.mode compact

# Optional parameters:
# @raycast.icon 🤖

# Documentation:
# @raycast.author Aki Suzuki
# @raycast.authorURL github.com/suzuki11109

launchctl unload /Library/LaunchAgents/com.paloaltonetworks.gp.pangp*
