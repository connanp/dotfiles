#!/bin/bash

readarray -t proptest_result <<<"$(/usr/bin/proptest -M i915 -D /dev/dri/card0 | grep -E 'Broadcast|Connector')"

for ((i = 0; i < ${#proptest_result[*]}; i += 2)); do
    connector=$(echo ${proptest_result[i]} | awk '{print $2}')
    connector_id=$(echo ${proptest_result[i + 1]} | awk '{print $1}')

    /usr/bin/proptest -M i915 $connector connector $connector_id 1
    # echo "$connector $connector_id"

done
