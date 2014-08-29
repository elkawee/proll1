

(module proll1 racket

	(require "macro_blocks.rkt" )
	(require "function_blocks.rkt" ) ; nur fuer goalify, das von function blocks direkt als macro exportiert wird 
	(provide =:=)
	(provide cond-e ) 
	(provide run* )
	(provide fresh)
	(provide and-g )
	(provide not-g )
	(provide goalify )
	(provide letq )
	(provide run ) 
)
