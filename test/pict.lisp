(require :stdlib *module-stdlib*)
(require :pict *module-pict*)

#|
PLATFORM: x86, ia64, amd64
CPUS:     Single, Dual, Quad
RAM:      128MB, 1GB, 4GB, 64GB
HDD:      SCSI, IDE
OS:       NT4, Win2K, WinXP, Win2K3
IE:       4.0, 5.0, 5.5, 6.0

# -------------------------------------------------------------------------
# Separate submodels for hardware and software parameters make the hardware
# configurations less variant lowering the cost of assembling the machines
# -------------------------------------------------------------------------

{ PLATFORM, CPUS, RAM, HDD } @ 2
{ OS, IE } @ 2

# -------------------------------------------------------------------------
# Constraints may cross submodel boundaries; here OS depends on PLATFORM
# -------------------------------------------------------------------------

IF [PLATFORM] in {"ia64", "amd64"} THEN [OS] in {"WinXP", "Win2K3"};

# ------------------------------------------------------------------------
# Constraints on parameters in the same submodel are also fine
# ------------------------------------------------------------------------

IF [PLATFORM] = "x86" THEN [RAM] <> "64GB";
|#

(defparameter factors
  '(("PLATFORM" "X86" "IA64" "AMD64")
    ("CPUS" "SINGLE" "DUAL" "QUAD")
    ("RAM" "128MB" "1GB" "4GB" "64GB")
    ("HDD" "SCSI" "IDE")
    ("OS" "XP" "WIN2K" "WINXP" "WIN2K3")
    ("IE" "4.0" "5.0" "5.5" "6.0")))

(defparameter models '(("OS" "IE")))

(defparameter conditions
  '((if (or (in "PLATFORM" "X86" "IA64") (and (= "RAM" "128MB") (> "IE" "4.0")))
      (= "HDD" "IDE"))))

(pict factors models conditions)
