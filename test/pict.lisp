(require :pict *module-pict*)

#|
    PLATFORM: X86, IA64, AMD64
    CPU: i3, i5, i7
    RAM1: 128MB, 1GB, 4GB, 64GB
    RAM2: 128MB, 1GB, 4GB, 64GB
    HDD: SCSI, IDE
    OS: xp, 7, 8, 8.1, 10
    IE: 4.0, 5.0, 5.5, 6.0

    # -------------------------------------------------------------------------
    # Separate submodels for hardware and software parameters make the hardware
    # configurations less variant lowering the cost of assembling the machines
    # -------------------------------------------------------------------------

    { PLATFORM, CPU, HDD } @ 2
    { OS, IE } @ 2

    # -------------------------------------------------------------------------
    # Constraints may cross submodel boundaries; here OS depends on PLATFORM
    # -------------------------------------------------------------------------

    IF [PLATFORM] IN {"IA64", "AMD64"} THEN [OS] IN {"WINXP", "WIN2K3"};

    # ------------------------------------------------------------------------
    # Constraints on parameters in the same submodel are also fine
    # ------------------------------------------------------------------------

    IF [PLATFORM] = "X86" THEN [RAM1] <> "64GB";
|#

(defparameter RAM '("128MB" "1GB" "4GB" "64GB"))
(defparameter params
  `((PLATFORM "X86" "IA64" "AMD64")
    (CPU "i3" "i5" "i7")
    (RAM1 ,@RAM)
    (RAM2 ,@RAM)
    (HDD "SCSI" "IDE")
    (OS "xp" "7" "8" "8.1" "10")
    (IE 4.0 5.0 5.5 6.0)))

(defparameter submodels '((2 PLATFORM CPU HDD)
                          (2 OS IE)))

(defparameter constraints
  '((if (or (in PLATFORM "X86" "IA64")
            (and (= RAM1 "128MB") (> IE 4.0)))
      (= HDD "IDE")
      (= HDD "SCSI"))
    (/= RAM1 RAM2)))

(pict params :submodels submodels :constraints constraints)
