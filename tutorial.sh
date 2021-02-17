#checks, if pv is installed
if ! command -v pv &> /dev/null; then
    echo "pv is not installed, but needed for the tutorial. Please install (i. e. through \"sudo apt install pv\")!"
    exit
fi

#checks, if autotool-helper is installed
if ! command autotool-helper &> /dev/null; then
    echo "autotool-helper could not be found. Please install (through \"stack install && stack install\") and make available in PATH variable!"
    exit
fi

echo "# Step 1: show available tasks" |pv -qL 12
sleep .2s
echo "> autotool-helper tasks" |pv -qL 12
sleep .2s
autotool-helper tasks
sleep 2s
clear

echo "# Step 2: show description for a specific task" |pv -qL 12
sleep .2s
echo "> autotool-helper help multisets" |pv -qL 12
sleep .2s
autotool-helper help multisets
sleep 2s
clear

echo "# Step 3: extract task description to file (and modify, if needed)" |pv -qL 12
sleep .2s
echo "> autotool-helper help multisets |tail -11 |head -9 > task.txt" |pv -qL 12
sleep .2s
autotool-helper help multisets |tail -11 |head -9 > task.txt
echo "> cat task.txt" |pv -qL 12
cat task.txt
sleep 2s
clear

echo "# Step 4: run task" |pv -qL 12
sleep .2s
echo "> autotool-helper multisets task.txt" |pv -qL 12
sleep .2s
autotool-helper multisets task.txt
sleep 5s
clear
