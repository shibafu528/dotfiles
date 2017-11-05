(defun kininarimasu ()
  (interactive)
  (message "私・・・、気になります!!!"))

(defun erutaso0 ()
  (interactive)	;; コマンドであると宣言する
  (setq num '(1))
  (while num
    (if (eq (car num) 1) (erutaso1)
      (erutaso2))
    (if (eq (car num) 1) (setq num '(2))
      (setq num '(1)))
    (if (sit-for 1) (setq num num)
      (setq num nil))))

(defun erutaso1 ()
  (interactive)	;; コマンドであると宣言する
  (let ((buffer (get-buffer-create "*erutaso*"))) ;; "*erutaso*"バッファを作成
  ;;(let ((buffer (generate-new-buffer "*erutaso*"))) ;; "*erutaso*"バッファを作成
    (set-buffer buffer) ;; "*erutaso*"バッファをカレントバッファにする
    (erase-buffer)
    (insert "::::,:/::|:::::|ｉ:::､::::::::::.\n") ;; カレントバッファに文字列を書き込む
    (insert ":::/:/:::|:::::||ﾍ:::::i:::::::::.\n")
    (insert "::/:/:::/|:::::|||::|::|:::::::::.\n")
    (insert ":::/⌒:/:|::::/|l⌒|::|::i::::::::.\n")
    (insert "::/＞´　 ￣￣　　 ｀＜|::|:::::::::.\n")
    (insert "／x＝ｭ､　　　　　　　ｨ＝ｭ､＼|:::ｉ:::::.\n")
    (insert ".／´　⌒　　　　　　　　　⌒ ＼|:::|:::::::.\n")
    (insert "{   {  rし}　 　 　 　  {  rし} j}|:::|ヽ::::::.\n")
    (insert "　　乂__ソ　　　　　　　 乂__ソ ' |:::|}:::::i\n")
    (insert "　　　　　　　　　　 　 　 　 　   |:::|ノ:::::|\n")
    (insert ":､:､:､:､:　　　〈j　　　 :､:､:､:､:､'|:::|:::::::|\n")
    (insert "　　　　　　　　　　　　　 　 ij   /|:::|:::::ｉ|:|\n")
    (insert "ﾍ　　 　 　 ｨ～-‐-､　　 ｛j/ .: :|:::|:::::ﾘ:|\n")
    (insert "::丶　 　　　　　 　 　 　 　 ｲ:::|:::|:::::/i|\n")
    (insert ":::: ＞　　　 　 　 　 イ::::::|:::|::::/　i|\n")
    (insert ":::::::| 　 ＞-＜　　 |:::::::|:::|:::/ jl\n")
    (insert "\n")
    (insert "     　　　わたし、\n")
    (insert "\n")
    (display-buffer (current-buffer)))) ;; カレントバッファを表示する

(defun erutaso2 ()
  (interactive)	;; コマンドであると宣言する
  (let ((buffer (get-buffer-create "*erutaso*"))) ;; "*erutaso*"バッファを作成
    ;; (let ((buffer (generate-new-buffer "*erutaso*"))) ;; "*erutaso*"バッファを作成
    (set-buffer buffer) ;; "*erutaso*"バッファをカレントバッファにする
    (erase-buffer)
    (insert ".             ／::::::::::::::::::::::::::::::＼\n")
    (insert "            / :::::::/:::::: i::::::::::':::::::ヽ\n")
    (insert "          ′:'::::::::/ :::::::ﾊ:::::::|:::'::::::::.\n")
    (insert "　　 　 /:::::/::::/:::/:::|:|::|:j:::{::::' :::::::.\n")
    (insert ".　 　 /:::::/::::,:::/孑|:| |:::l::|:ﾄ:::::'::'.::::.\n")
    (insert "      /:::::/j|:::|:::{|| ″'´⌒｀└乢::}  Ｖ|i::::'.:::.\n")
    (insert ".    /:::::/::|:::|:/,z==ミ 　　　 ,z==ミ､り!:::}i:::::.\n")
    (insert "    ,:::::/:i:|:::l〃,:⌒:, 　　　  ,:⌒い= |:::|:i:i:::.\n")
    (insert "    .::: /,:i:|:::代 乂_,ﾉ　　　　　乂_,ﾉﾉ. |:::|:i:i:i::.\n")
    (insert "    ::://:i:i(|:::|　　¨¨　　　　　　 ¨¨　  |:::|)::i:i::い\n")
    (insert "    :://:::i:j|:::|、 ''' 　　′    　 '''    :::|:i:i:i:iﾊ::.\n")
    (insert "    :/  :i:i:i|:::|ﾊ        r―――┐　　 　  /::::, i:i:i:iﾄ:!‘,\n")
    (insert ".   // j:j:i:i|:::|i:i丶.　 ゝ- ′　 　 ィ′:::::, i:i:i:i:} li ‘\n")
    (insert ".  //! !:i:i:j|:::V:i:i 〕.,　　,. [i:i:i,:::, i:i:i:i:i} l| i\n")
    (insert "  || | l:i:i:i{::::V  ‐ ｝ ｀ ´ 　 {､i:,::::, i:i:i:i:i:iﾉl| }\n")
    (insert "　|| 　{.-{ﾆﾆﾆ}:,:::V{:ﾉ　　　 　 　ゝ}:::::, ﾆﾆﾆ７ ､j!　ji\n")
    (insert "　||／ 　 ∨ニ∧::::}ﾆ{　　　　　 　 /{::::::::::′ﾆﾆ/ 　 ＼ﾉ\n")
    (insert ". / 　 ヽ.　∨ニ∧:j}=∧- ＿＿＿＿ -/=}:::::::/ﾆﾆ／　　/　‘,\n")
    (insert "\n")
    (insert "                       気になります！\n")
    (insert "\n")
    (display-buffer (current-buffer)))) ;; カレントバッファを表示する

(provide 'chitanda)
