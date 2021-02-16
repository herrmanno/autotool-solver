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
