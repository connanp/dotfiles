prompt Loading login.sql file...

store set sqlplus-default.sql replace

ALTER SESSION SET NLS_DATE_FORMAT         = 'YYYY-MM-DD HH24:MI:SS';
ALTER SESSION SET NLS_TIMESTAMP_FORMAT    = 'YYYY-MM-DD HH24:MI:SS.FF';
ALTER SESSION SET NLS_TIMESTAMP_TZ_FORMAT = 'YYYY-MM-DD HH24:MI:SS.FF TZH:TZM';

--@prompt
-- SET SQLPROMPT "_DATE _USER'@'_CONNECT_IDENTIFIER _PRIVILEGE> ";



-- *SET SHOW[MODE] {ON | OFF}
-- 	Controls whether SQL*Plus lists the old and new settings of a SQL*Plus system variable when you change the setting with SET.

--SET SHOWMODE ON

-- SET APPI[NFO]{ON | OFF | text}
-- 	Sets automatic registering of scripts through the DBMS_APPLICATION_INFO package.
-- SET ARRAY[SIZE] {15 | n}
-- 	Sets the number of rows, called a batch, that SQL*Plus will fetch from the database at one time.

--SET ARRAYSIZE 5000

-- SET AUTO[COMMIT]{ON | OFF | IMM[EDIATE] | n}
-- 	Controls when Oracle Database commits pending changes to the database.

SET AUTOCOMMIT OFF

-- SET AUTOP[RINT] {ON | OFF}
-- 	Sets the automatic printing of bind variables.
-- SET AUTORECOVERY [ON | OFF]
-- 	ON sets the RECOVER command to automatically apply the default filenames of archived redo log files needed during recovery.
-- SET AUTOT[RACE] {ON | OFF | TRACE[ONLY]} [EXP[LAIN]] [STAT[ISTICS]]
-- 	Displays a report on the execution of successful SQL DML statements (SELECT, INSERT, UPDATE or DELETE).
-- SET BLO[CKTERMINATOR] {. | c | ON | OFF}
-- 	Sets the non-alphanumeric character used to end PL/SQL blocks to c.
-- SET CMDS[EP] {; | c | ON | OFF}
-- 	Sets the non-alphanumeric character used to separate multiple SQL*Plus commands entered on one line to c.
-- SET COLSEP { | text}
-- 	In iSQL*Plus, SET COLSEP determines the column separator character to be printed between column output that is rendered inside <PRE> tags.
-- Sets the text to be printed between selected columns.

SET COLSEP |

-- SET CON[CAT] {. | c | ON | OFF}
-- 	Sets the character you can use to terminate a substitution variable reference if you wish to immediately follow the variable with a character that SQL*Plus would otherwise interpret as a part of the substitution variable name.
-- SET COPYC[OMMIT] {0 | n}
-- 	Controls the number of batches after which the COPY command commits changes to the database.
-- SET COPYTYPECHECK {ON | OFF}
-- 	Sets the suppression of the comparison of datatypes while inserting or appending to tables with the COPY command.
-- SET DEF[INE] {& | c | ON | OFF}
-- 	Sets the character used to prefix variables to c.
-- SET DESCRIBE [DEPTH {1 | n | ALL}] [LINENUM {ON | OFF}] [INDENT {ON | OFF}]
-- 	Sets the depth of the level to which you can recursively describe an object.
-- *SET EDITF[ILE] file_name[.ext]
-- 	Sets the default filename for the EDIT command.
-- SET EMB[EDDED] {ON | OFF}
-- 	Controls where on a page each report begins.
-- SET ESC[APE] {\ | c | ON | OFF}
-- 	Defines the character you enter as the escape character.
-- SET FEED[BACK] {6 | n | ON | OFF}
-- 	Displays the number of records returned by a query when a query selects at least n records.

SET FEEDBACK ON

-- SET FLAGGER {OFF | ENTRY | INTERMED[IATE] | FULL}
-- 	Checks to make sure that SQL statements conform to the ANSI/ISO SQL92 standard.
-- *SET FLU[SH] {ON | OFF}
-- 	Controls when output is sent to the user's display device.
-- SET HEA[DING] {ON | OFF}
-- 	Controls printing of column headings in reports.
-- SET HEADS[EP] { | | c | ON | OFF}
-- 	Defines the character you enter as the heading separator character.
-- SET INSTANCE [instance_path | LOCAL]
-- 	Changes the default instance for your session to the specified instance path.
-- SET LIN[ESIZE] {80 | n} SET LIN[ESIZE] {150 | n} in iSQL*Plus
-- 	Sets the total number of characters that SQL*Plus displays on one line before beginning a new line.

SET LINESIZE 500

-- SET LOBOF[FSET] {1 | n}
-- 	Sets the starting position from which CLOB and NCLOB data is retrieved and displayed.
-- SET LOGSOURCE [pathname]
-- 	Specifies the location from which archive logs are retrieved during recovery.
-- SET LONG {80 | n}
-- 	Sets maximum width (in bytes) for displaying LONG, CLOB, NCLOB and XMLType values; and for copying LONG values.

SET LONG 10000
-- max is SET LONG 2000000000

-- SET LONGC[HUNKSIZE] {80 | n}
-- 	Sets the size (in bytes) of the increments in which SQL*Plus retrieves a LONG, CLOB, NCLOB or XMLType value.

SET LONGCHUNKSIZE 10000
-- max is SET LONGCHUNKSIZE 2000000000

-- SET MARK[UP] HTML [ON | OFF] [HEAD text] [BODY text] [TABLE text] [ENTMAP {ON | OFF}] [SPOOL {ON | OFF}] [PRE[FORMAT] {ON | OFF}]
-- 	Outputs HTML marked up text, which is the output used by iSQL*Plus.

set markup HTML OFF HEAD "<style type='text/css'> body, p, table,tr,td {font:10pt Consolas,monospace; color:black; background:White;} table,tr,td { background:#f7f7e7; padding:0px 0px 0px 0px; margin:0px 0px 0px 0px;} th {font:bold 10pt Arial,Helvetica,sans-serif; color:#336699; background:#cccc99; padding:0px 0px 0px 0px;} h1 {font:16pt Arial,Helvetica,Geneva,sans-serif; color:#336699; background-color:White; border-bottom:1px solid #cccc99; margin-top:0pt; margin-bottom:0pt; padding:0px 0px 0px 0px;} h2 {font:bold 10pt Arial,Helvetica,Geneva,sans-serif; color:#336699; background-color:White; margin-top:4pt; margin-bottom:0pt;} a { color:#663300; background:#ffffff; margin-top:0pt; margin-bottom:0pt; vertical-align:top;}</style><title>SQL*Plus Report</title>" BODY "" TABLE "border='1' summary='Script output'" SPOOL OFF ENTMAP OFF PRE OFF

-- SET NEWP[AGE] {1 | n | NONE}
-- 	Sets the number of blank lines to be printed from the top of each page to the top title.
-- SET NULL text
-- 	Sets the text that represents a null value in the result of a SQL SELECT command.

SET NULL '~'

-- SET NUMF[ORMAT] format
-- 	Sets the default format for displaying numbers.
-- SET NUM[WIDTH] {10 | n}
-- 	Sets the default width for displaying numbers.

-- 64 bit ID = 20 digits + 1 extra for minus sign
SET NUMWIDTH 21
-- set this to numwidth 38 if you have huge numeric keys

-- SET PAGES[IZE] {14 | n}
-- 	Sets the number of lines in each page.
-- 	Enables you to control scrolling of your terminal when running reports.
-- You can set PAGESIZE to zero to suppress all headings, page breaks, titles, the initial blank line, and other formatting information.

SET PAGESIZE 50000

-- SET RECSEP {WR[APPED] | EA[CH] | OFF}
-- 	RECSEP tells SQL*Plus where to make the record separation.
-- SET RECSEPCHAR { | c}
-- 	Display or print record separators.
-- SET SERVEROUT[PUT] {ON | OFF} [SIZE {n | UNL[IMITED]}] [FOR[MAT] {WRA[PPED] | WOR[D_WRAPPED] | TRU[NCATED]}]
-- 	Controls whether to display the output (that is, DBMS_OUTPUT PUT_LINE) of stored procedures or PL/SQL blocks in SQL*Plus.
-- *SET SHIFT[INOUT] {VIS[IBLE] | INV[ISIBLE]}
-- 	Enables correct alignment for terminals that display shift characters.
-- *SET SQLBL[ANKLINES] {ON | OFF}
-- 	Controls whether SQL*Plus puts blank lines within a SQL command or script.
-- SET SQLC[ASE] {MIX[ED] | LO[WER] | UP[PER]}
-- 	Converts the case of SQL commands and PL/SQL blocks just prior to execution.
-- *SET SQLCO[NTINUE] {> | text}
-- 	Sets the character sequence SQL*Plus displays as a prompt after you continue a SQL*Plus command on an additional line using a hyphen (.).
-- *SET SQLN[UMBER] {ON | OFF}
-- 	Sets the prompt for the second and subsequent lines of a SQL command or PL/SQL block.
-- SET SQLPLUSCOMPAT[IBILITY] {x.y[.z]}
-- 	Sets the behavior or output format of VARIABLE to that of the release or version specified by x.y[.z].
-- *SET SQLPRE[FIX] {# | c}
-- 	Sets the SQL*Plus prefix character.
-- *SET SQLP[ROMPT] {SQL> | text}
-- 	Sets the SQL*Plus command prompt.
-- SET SQLT[ERMINATOR] {; | c | ON | OFF}
-- 	Sets the character used to end and execute SQL commands to c.
-- *SET SUF[FIX] {SQL | text}
-- 	Sets the default file that SQL*Plus uses in commands that refer to scripts.
-- *SET TAB {ON | OFF}
-- 	Determines how SQL*Plus formats white space in terminal output.

--SET TAB ON

-- *SET TERM[OUT] {ON | OFF}
-- 	Controls the display of output generated by commands executed from a script.
-- *SET TI[ME] {ON | OFF}
-- 	Controls the display of the current time.

--SET TIME ON

-- SET TIMI[NG] {ON | OFF}
-- 	Controls the display of timing statistics.
-- *SET TRIM[OUT] {ON | OFF}
-- 	Determines whether SQL*Plus puts trailing blanks at the end of each displayed line.

SET TRIMOUT ON

-- *SET TRIMS[POOL] {ON | OFF}
-- 	Determines whether SQL*Plus puts trailing blanks at the end of each spooled line.

SET TRIMSPOOL ON

-- SET UND[ERLINE] {- | c | ON | OFF}
-- 	Sets the character used to underline column headings in SQL*Plus reports to c.
-- SET VER[IFY] {ON | OFF}
-- 	Controls whether SQL*Plus lists the text of a SQL statement or PL/SQL command before and after SQL*Plus replaces substitution variables with values.
-- SET WRA[P] {ON | OFF}
-- 	Controls whether SQL*Plus truncates the display of a SELECTed row if it is too long for the current line width.

--SET WRAP OFF

-- SET XQUERY BASEURI {text}
-- 	Defines the base URI to use. This is useful to change the prefix of the file to access when writing generic XQuery expressions.
-- SET XQUERY ORDERING {UNORDERED | ORDERED | DEFAULT}
-- 	Controls the ordering of results from an XQuery.
-- SET XQUERY NODE {BYVALUE | BYREFERENCE | DEFAULT}
-- 	Sets the preservation mode for notes created or returned.
-- SET XQUERY CONTEXT {text}
-- 	Specifies an XQuery context item which can be either a node or a value.

-- SET ECHO {ON | OFF}
-- 	Controls whether the START command lists each command in a script as the command is executed.

SET ECHO ON

SET SHOWMODE ON
