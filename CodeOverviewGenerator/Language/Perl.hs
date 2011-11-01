-- | Define some perl coloration, only basic keywords are highlighted
module CodeOverviewGenerator.Language.Perl( perlCodeDef ) where

import Control.Applicative
{-import qualified Data.Map as M-}
import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Color
{-import qualified CodeOverviewGenerator.ByteString as B-}

-- | Perl tokens
perlConditional, perlRepeat :: [String]
perlConditional = ["if", "elsif", "unless", "given", "when", "default", "else"]
perlRepeat = ["while", "for", "each", "do", "until", "continue"]

{-syn match perlOperator			"\<\%(defined\|undef\|eq\|ne\|[gl][et]\|cmp\|not\|and\|or\|xor\|not\|bless\|ref\|do\)\>"-}
{-syn match perlStatementStorage		"\<\%(my\|our\|local\|state\)\>"-}
{-syn match perlStatementControl		"\<\%(return\|last\|next\|redo\|goto\|break\)\>"-}
{-syn match perlStatementScalar		"\<\%(chom\=p\|chr\|crypt\|r\=index\|lc\%(first\)\=\|length\|ord\|pack\|sprintf\|substr\|uc\%(first\)\=\)\>"-}
{-syn match perlStatementRegexp		"\<\%(pos\|quotemeta\|split\|study\)\>"-}
{-syn match perlStatementNumeric		"\<\%(abs\|atan2\|cos\|exp\|hex\|int\|log\|oct\|rand\|sin\|sqrt\|srand\)\>"-}
{-syn match perlStatementList		"\<\%(splice\|unshift\|shift\|push\|pop\|join\|reverse\|grep\|map\|sort\|unpack\)\>"-}
{-syn match perlStatementHash		"\<\%(delete\|each\|exists\|keys\|values\)\>"-}
{-syn match perlStatementIOfunc		"\<\%(syscall\|dbmopen\|dbmclose\)\>"-}
{-syn match perlStatementFiledesc		"\<\%(binmode\|close\%(dir\)\=\|eof\|fileno\|getc\|lstat\|printf\=\|read\%(dir\|line\|pipe\)\|rewinddir\|say\|select\|stat\|tell\%(dir\)\=\|write\)\>" nextgroup=perlFiledescStatementNocomma skipwhite-}
{-syn match perlStatementFiledesc		"\<\%(fcntl\|flock\|ioctl\|open\%(dir\)\=\|read\|seek\%(dir\)\=\|sys\%(open\|read\|seek\|write\)\|truncate\)\>" nextgroup=perlFiledescStatementComma skipwhite-}
{-syn match perlStatementVector		"\<vec\>"-}
{-syn match perlStatementFiles		"\<\%(ch\%(dir\|mod\|own\|root\)\|glob\|link\|mkdir\|readlink\|rename\|rmdir\|symlink\|umask\|unlink\|utime\)\>"-}
{-syn match perlStatementFiles		"-[rwxoRWXOezsfdlpSbctugkTBMAC]\>"-}
{-syn match perlStatementFlow		"\<\%(caller\|die\|dump\|eval\|exit\|wantarray\)\>"-}
{-syn match perlStatementInclude		"\<require\>"-}
{-syn match perlStatementInclude		"\<\%(use\|no\)\s\+\%(\%(attributes\|attrs\|autouse\|parent\|base\|big\%(int\|num\|rat\)\|blib\|bytes\|charnames\|constant\|diagnostics\|encoding\%(::warnings\)\=\|feature\|fields\|filetest\|if\|integer\|less\|lib\|locale\|mro\|open\|ops\|overload\|re\|sigtrap\|sort\|strict\|subs\|threads\%(::shared\)\=\|utf8\|vars\|version\|vmsish\|warnings\%(::register\)\=\)\>\)\="-}
{-syn match perlStatementProc		"\<\%(alarm\|exec\|fork\|get\%(pgrp\|ppid\|priority\)\|kill\|pipe\|set\%(pgrp\|priority\)\|sleep\|system\|times\|wait\%(pid\)\=\)\>"-}
{-syn match perlStatementSocket		"\<\%(acept\|bind\|connect\|get\%(peername\|sock\%(name\|opt\)\)\|listen\|recv\|send\|setsockopt\|shutdown\|socket\%(pair\)\=\)\>"-}
{-syn match perlStatementIPC		"\<\%(msg\%(ctl\|get\|rcv\|snd\)\|sem\%(ctl\|get\|op\)\|shm\%(ctl\|get\|read\|write\)\)\>"-}
{-syn match perlStatementNetwork		"\<\%(\%(end\|[gs]et\)\%(host\|net\|proto\|serv\)ent\|get\%(\%(host\|net\)by\%(addr\|name\)\|protoby\%(name\|number\)\|servby\%(name\|port\)\)\)\>"-}
{-syn match perlStatementPword		"\<\%(get\%(pw\%(uid\|nam\)\|gr\%(gid\|nam\)\|login\)\)\|\%(end\|[gs]et\)\%(pw\|gr\)ent\>"-}
{-syn match perlStatementTime		"\<\%(gmtime\|localtime\|time\)\>"-}

{-syn match perlStatementMisc		"\<\%(warn\|formline\|reset\|scalar\|prototype\|lock\|tied\=\|untie\)\>"-}

-- | Perl langage definition
perlCodeDef :: CodeDef [CodeEntity]
perlCodeDef = def
    where def = CodeDef
           { lineComm = strComment "//"
           , multiLineCommBeg = strComment "/*"
           , multiLineCommEnd = strComment "*/"
           , recursiveComment = False
           , tabSpace = 4
           , identParser = identWithPrime
           , strParser = Just $ stringParser False def
           , specialIdentifier = prepareKeywords
                [ (perlRepeat, RepeatEntity)
                , (perlConditional, ConditionalEntity)
                ]
           , specificParser = [intParser]
           , heatTokens = 
                [ heatToken    1 '{'
                , heatToken (-1) '}'
                , heatToken    1 '('
                , heatToken (-1) ')'
                , heatToken    1 '['
                , heatToken (-1) ']'
                ]
           }
           where heatToken i c = return (1, i) <$> charParse c

