#!/usr/bin/env sh

files() {
  echo "1  a280"
  echo "2  ali535"
  echo "3  berlin52"
  echo "4  burma14"
  echo "5  gr137"
  echo "6  gr202"
  echo "7  gr229"
  echo "8  gr431"
  echo "9  gr666"
  echo "10 gr96"
  echo "11 pr226"
  echo "12 u574"
  echo "13 ulysses16"
  echo "14 ulysses22"
}

name() {
  case $1 in
    1  ) echo "a280Distance.txt" ;;
    2  ) echo "ali535Distances.txt" ;;
    3  ) echo "berlin52Distance.txt" ;;
    4  ) echo "burma14Distances.txt" ;;
    5  ) echo "gr137Distances.txt" ;;
    6  ) echo "gr202Distances.txt" ;;
    7  ) echo "gr229Distances.txt" ;;
    8  ) echo "gr431Distances.txt" ;;
    9  ) echo "gr666Distances.txt" ;;
    10 ) echo "gr96Distances.txt" ;;
    11 ) echo "pr226Distance.txt" ;;
    12 ) echo "u574Distance.txt" ;;
    13 ) echo "ulysses16Distances.txt" ;;
    14 ) echo "ulysses22Distances.txt" ;;
  esac
}

tour() {
  case $1 in
    1  ) echo "a280" ;;
    2  ) echo "ali535" ;;
    3  ) echo "berlin52" ;;
    4  ) echo "burma14" ;;
    5  ) echo "gr137" ;;
    6  ) echo "gr202" ;;
    7  ) echo "gr229" ;;
    8  ) echo "gr431" ;;
    9  ) echo "gr666" ;;
    10 ) echo "gr96" ;;
    11 ) echo "pr226" ;;
    12 ) echo "u574" ;;
    13 ) echo "ulysses16" ;;
    14 ) echo "ulysses22" ;;
  esac
}

while true; do
  files
  read -p "Choose a file number or <q> to quit: " num
  if (( 1 <= num && num <= 14 )); then
    sed "s/PUT_THE_FILENAME_HERE/\"$(name $num)\"/" subtours_elimination_fractional_4.tmpl.zpl > subtours_elimination_fractional_4.zpl
    scip -f subtours_elimination_fractional_4.zpl > "$(tour $num)_elimination_fractional_4.log"
  elif [[ "$num" == q* ]]; then
    exit
  else
    echo "That is not valid"
  fi
  echo
done
