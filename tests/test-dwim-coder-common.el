;; -*- lexical-binding:t -*-

(load "./dwim-coder-common.el")

(require 'buttercup)
(require 'cl-lib)
(require 'dwim-coder-common)

(describe "dwim-coder-s-to-style"

  (it "snake string"
    (let ((values
           '(("Hello" . "hello")
             ("hello" . "hello")
             ("_h_e_l_l_o_" . "_h_e_l_l_o_")
             ("Hello world". "hello_world")
             ("HelloWorld" . "hello_world")
             ("_HelloWorld" . "_hello_world")
             ("hello::world" . "hello::world")
             ("hello_world " . "hello_world ")
             ("hello_good world" . "hello_good_world")
             ("_hello world" . "_hello_world"))))
      (cl-loop for (key . value) in values
               collect (expect (dwim-coder-s-to-style key "snake") :to-equal value))))

  (it "camel string"
    (let ((values
           '(("hello" . "Hello")
             ("HelloWorld" . "HelloWorld")
             ("_HelloWorld" . "_HelloWorld")
             ("_HelloWorld_" . "_HelloWorld_")
             ("n m client" . "NMClient")
             ("g_d_bus proxy" . "GDBusProxy"))))
      (cl-loop for (key . value) in values
               collect (expect (dwim-coder-s-to-style key "upcamel") :to-equal value))))

  (it "cycle string"
    (let ((values
           '(("hello" . "HELLO")
             ("_hello" . "_HELLO")
             ("hello_WORLD" . "HELLO_WORLD")
             ("HELLO_world" . "HELLO_WORLD")
             ("HELLOworld" . "HELLOWORLD")
             ("H_E_L_L_o" . "H_E_L_L_O")
             ("H_ello" . "H_ELLO")
             ("H_EL_lo" . "H_EL_LO")
             ("HelloWorld" . "hello_world")
             ("GFile" . "g_file")
             ("HELLOWORLD" . "Helloworld")
             ("HELLO_WORLD" . "HelloWorld"))))
      (cl-loop for (key . value) in values
               collect (expect (dwim-coder-s-to-style key "cycle") :to-equal value)))))
