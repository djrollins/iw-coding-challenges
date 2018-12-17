#!/bin/bash

FILE="/tmp/chat-file"

USERNAME=$1

# Create a file to store all the messages
if [ ! -f $FILE ]; then
	touch $FILE
fi

# Follow the file, ignoring history, to read messages and background it.
tail -n 0 -f $FILE &
TAIL_PID=$!

quit() {
	kill $TAIL_PID
	exit
}

trap quit INT

# Read stdin and append it to the messages file
while read line; do
	case line in
		/quit|/q)
			quit
			;;
		/history|/h)
			cat $FILE
			;;
		*)
			echo "$USERNAME: $line" >> $FILE
			;;
	esac
done
