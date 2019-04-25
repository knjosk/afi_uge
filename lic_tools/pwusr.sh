#!/bin/bash
LM_PATH="/work/A/tools"
LM_CMD="$LM_PATH/rlmutil rlmstat -a -c "
PORT=5153
LM_HOST="afimge001-m"

${LM_CMD} ${PORT}@${LM_HOST}
