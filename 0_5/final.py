"""
File: final.py
Authors:
  Dor Rondel
  Nirender
Instructor: Prof. Daniel Schlegel
Course: CSc 344
Institution: SUNY Oswego
"""

import os
import re
import zipfile
import smtplib
import getpass
from email import encoders
from email.mime.multipart import MIMEMultipart
from email.mime.base import MIMEBase
from email.mime.text import MIMEText

###################################
######## Symbol Pair Maker ########
###################################

def is_c_symbol(line):
    ''' Filters valid C symbols, returns symbol if valid'''
    line = line.lstrip()

    if (line[0:6] == "struct") and (line[7:11] == "node") and '{' in line:  # struct _____ { ... }
        return line[7:line.index('{')-1]
    elif (line[0:5] == "const") and (line[6:10] == "char"):  # const char *_____;
        return line[12:line.index(';')]
    elif (line[0:6] == "struct") and (line[7:11] == "node") and ('*' in line):  # struct node *______;
        line = line.replace("[64]", "")
        return line[line.index('*')+1:line.index(';')]
    elif (line[0:4] == "void"):  # void ___ () {...}
        return line[5:line.index('(')]
    elif (line[0:3] == "int") and ('=' not in line):  # int ____() {...}
        return line[4:line.index('(')]
    elif (line[0:3] == "int") and ('=' in line):  # int ____ = ...;
        return line[4:line.index('=')-1]
    elif (line[0:4] == "bool"):  # bool ____(){}
        return line[5:line.index('(')]
    elif (line[0:4] == "char"):  # char ____[0-9];
        return line[5:line.index('[')]
    elif (line[0:3] == "DIR"):  # DIR  *____;
        return line[5:line.index(';')]
    elif (line[0:6] == "struct") and (line[7:13] == "dirent"):  # struct dirent *____;
        return line[15:line.index(';')]
    else:
        pass

def is_clj_symbol(line):
    ''' Filters valid Clojure identifiers and returns them if they are '''
    line = line.lstrip()

    if (line[0:3] == "(ns"):   # (ns ____)
        return line[4:line.index(')')]
    elif (line[0:5] == "(defn"):  # (defn ____)
        return line[6:line.index('[')-1]
    elif (line[0:4] == "(let"):  #  (let [[x & y] [...]])
        return [line[7:line.index('&')-1], line[line.index('&')+2:line.index(']')]]
    else:
        pass

def is_hs_symbol(line):
    ''' Filters valid Haskell identifiers and returns them if they are '''
    line = line.lstrip()

    if line:  # if line not empty... was giving index error
        if (line[0:4] == "data"):  # data ____ = ____ { ___ :: ... }
            return (line[5:line.index('=')-1], line[line.index('=')+2:line.index('{')-1], line[line.index('{')+2:line.index(':')-1])
        elif (line[0] == "|"):  # | ____ { __ :: ... }
            return [line[2:line.index('{')-1], line[line.index('{')+2: line.index(':')-1]]
        elif ("::" in line):  # _____ :: ...
            return line[:line.index(":")-1]
        elif ("<-" in line):  # ____ <- ...
            return line[:line.index("<")-1]
        else:
            pass

def is_pl_symbol(line):
    ''' Filters valid Prolog identifiers and returns them if they are '''
    line = line.lstrip()

    if (line[:2] == ":-"):  # :- ____(
        return line[3:line.index('(')]
    elif ('(' in line) and (')' in line) and ('.' in line) and (','not in line):  # ____().
        return line[:line.index('(')]
    elif ('(' in line) and (')' in line) and ('.' in line) and (',' in line):  # ____().
        return line[line.index(','):line.index('(')]
    elif (":-" in line) and (line[:2] != ":-") and ('(' not in line):  # ____ :-
        return line[:line.index(':')]
    elif (":-" in line) and (line[:2] != ":-") and ('(' in line):  # ____() :-
        return line[:line.index('(')]
    else:
        pass

def capital_letter_pl(line):
    ''' returns prolog variable names in a list'''
    pattern = re.compile(r"[A-Z]+[a-z]*")
    return pattern.findall(line)

def is_py_symbol(line):
    ''' Filters valid Python identifiers and returns them if they are '''
    line = line.lstrip()

    if (line[0:3] == "def"):   # def ____(
        return line[4:line.index('(')]
    elif ('=' in line) and (line.count("=") == 1) and ('if' not in line) and ('return' not in line) and ('<' not in line):  # ___ = ...
        return line[:line.index('=')-1]
    else:
        pass

symbol_list = []
def get_symbol_pair():
    ''' makes [<lang>, <id>] tuple and writes to file '''
    taken = []

    tmp = os.getcwd()
    os.chdir('..')
    directory = os.getcwd()

    # C
    with open("0_1/filecomplete.c") as c_file:
        for line in c_file:
            if (is_c_symbol(line) != None) and (is_c_symbol(line) not in taken):
                symbol_list.append("[C, {}]".format(is_c_symbol(line)))
                taken.append(is_c_symbol(line))

    # Clojure
    with open("0_2/hw2.clj") as clj_file:
        for line in clj_file:
            if (is_clj_symbol(line) != None) and (is_clj_symbol(line) not in taken):
                if type(is_clj_symbol(line)) == list:
                    symbol_list.append("[Clojure, {}]".format(is_clj_symbol(line)[0]))
                    symbol_list.append("[Clojure, {}]".format(is_clj_symbol(line)[1]))
                    taken.append(is_clj_symbol(line)[0])
                    taken.append(is_clj_symbol(line)[1])
                else:
                    symbol_list.append("[Clojure, {}]".format(is_clj_symbol(line)))
                    taken.append(is_clj_symbol(line))

    # Haskell
    with open("0_3/hw3.hs") as hs_file:
        for line in hs_file:
            if (is_hs_symbol(line) != None):
                if type(is_hs_symbol(line)) == list:
                    if is_hs_symbol(line)[0] not in taken:
                        symbol_list.append("[Haskell, {}]".format(is_hs_symbol(line)[0]))
                        taken.append(is_hs_symbol(line)[0])
                    if is_hs_symbol(line)[1] not in taken:
                        symbol_list.append("[Haskell, {}]".format(is_hs_symbol(line)[1]))
                        taken.append(is_hs_symbol(line)[1])
                elif type(is_hs_symbol(line)) == tuple:
                    symbol_list.append("[Haskell, {}]".format(is_hs_symbol(line)[0]))
                    symbol_list.append("[Haskell, {}]".format(is_hs_symbol(line)[1]))
                    symbol_list.append("[Haskell, {}]".format(is_hs_symbol(line)[2]))
                    taken.append(is_hs_symbol(line)[0])
                    taken.append(is_hs_symbol(line)[1])
                    taken.append(is_hs_symbol(line)[2])
                else:
                    if is_hs_symbol(line) not in taken:
                        symbol_list.append("[Haskell, {}]".format(is_hs_symbol(line)))
                        taken.append(is_hs_symbol(line))

    # Prolog
    with open("0_4/tmp.pl") as pro_file:
        for line in pro_file:
            if (is_pl_symbol(line) != None) and (is_pl_symbol(line) not in taken):
                symbol_list.append("[Prolog, {}]".format(is_pl_symbol(line)))
                taken.append(is_pl_symbol(line))
            if capital_letter_pl(line) != None:
                for word in capital_letter_pl(line):
                    if word not in taken:
                        symbol_list.append("[Prolog, {}]".format(word))
                        taken.append(word)

    # Python
    with open("0_5/final.py") as py_file:
        for line in py_file:
            if (is_py_symbol(line) != None) and (is_py_symbol(line) not in taken):
                symbol_list.append("[Python, {}]".format(is_py_symbol(line)))
                taken.append(is_py_symbol(line))


    # Make Symbol File
    with open("symbols.txt","w+") as write_file:
        for pair in symbol_list:
            write_file.write(pair+"\n")


###################################
######## HTML Generator    ########
###################################

def make_html():
    ''' generates HTML file for given specifications '''
    string = """<!DOCTYPE html>
    <html>
    <head>
        <title> CSc 344 - Portfolio </title>
        <style>
            h2 {
                text-align: center;
            }

            ul {
                margin: 0 auto;
            }
        </style>
    </head>
    <body>
        <h2><u><em>Dor and Nirender CSc 344 Portfolio</em></u></h2>
        <div id="main">
            <h4> Source Code: </h4>
            <br/>
            <ul>
                <li>
                    <a href="./0_1/filecomplete.c">C</a>
                </li>
                <li>
                    <a href="./0_2/hw2.clj">Clojure</a>
                </li>
                <li>
                    <a href="./0_3/hw3.hs">Haskell</a>
                </li>
                <li>
                    <a href="./0_4/tmp.pl">Prolog</a>
                </li>
                <li>
                    <a href="./0_5/final.py">Python</a>
                </li>
                <li>
                    <a href="./symbols.txt">Cumulative Symbols File</a>
                </li>
            </ul>
        </div>
    </body>
    </html>
    """
    with open("symbols.html","w+") as write_file:
        write_file.write(string)


###################################
######## Executable Zipper ########
###################################

def zip_files():
    ''' Zips appropriate files together '''
    zipper = zipfile.ZipFile('class.zip', 'w')
    path = "./"
    fnames = ['filecomplete.c', 'hw2.clj', 'hw3.hs', 'tmp.pl', 'final.py', 'symbols.txt', 'symbols.html']
    for root, dirs, files in os.walk(path):
        for file in files:
            for name in fnames:
                if name == file:
                    zipper.write(os.path.join(root, file))

###################################
######## E-mail Sender     ########
###################################

def email_zip():
    ''' Sends email of zip file made '''
    sender = 'drondel@oswego.edu'
    receiver = raw_input("Email address of .zip file recipient: ")
    password = getpass.getpass()

    message = MIMEMultipart()
    message["From"] = sender
    message["To"] = receiver
    message["Subject"] = "[CSC 344 - Dor Rondel] HW 5"
    message.attach(MIMEText("Zip of course work"))
    part = MIMEBase('application', 'octet-stream')
    part.set_payload(open("class.zip", 'rb').read())
    encoders.encode_base64(part)
    part.add_header('Content-Disposition',
                    'attachment; filename="class.zip"')
    message.attach(part)

    try:
       smtpObj = smtplib.SMTP("smtp.gmail.com", 587)
       smtpObj.ehlo(); smtpObj.starttls(); smtpObj.ehlo();
       smtpObj.login(sender, password)
       smtpObj.sendmail(sender, receiver, message.as_string())
       smtpObj.close()
       print("Success!")
    except smtplib.SMTPException:
       print("Error: unable to send email")

###################################
########### Main Loop   ###########
###################################

def main():
    ''' main fn '''
    get_symbol_pair()
    make_html()
    zip_files()
    email_zip()

if __name__ == '__main__':
    main()
