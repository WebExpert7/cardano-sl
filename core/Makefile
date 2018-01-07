
#	~/experiments/text/Data/Text/Internal/Char.hs
#	-XNoImplicitPrelude 


	# ~/experiments/text/Data/Text/Internal/Encoding/Utf16.hs \
	# ~/experiments/text/Data/Text/Internal/Unsafe.hs \
	# ~/experiments/text/Data/Text/Internal/Unsafe/Shift.hs \
	# ~/experiments/text/Data/Text/Array.hs \
	# ~/experiments/text/Data/Text/Internal/Unsafe/Char.hs \
	# ~/experiments/text/Data/Text/Internal.hs \
	# ~/experiments/text/Data/Text/Unsafe.hs \
	# ~/experiments/text/Data/Text/Internal/Private.hs \
	# ~/experiments/text/Data/Text/Internal/Fusion/Size.hs \
	# ~/experiments/text/Data/Text/Internal/Fusion/Types.hs \
	# ~/experiments/text/Data/Text/Internal/Fusion/CaseMapping.hs \
	# ~/experiments/text/Data/Text/Internal/Fusion/Common.hs \
	# ~/experiments/text/Data/Text/Internal/Fusion.hs \
	# ~/experiments/text/Data/Text/Encoding/Error.hs \
	# ~/experiments/text/Data/Text/Show.hs \
	# ~/experiments/text/Data/Text/Internal/Encoding/Fusion/Common.hs \
	# ~/experiments/text/Data/Text/Internal/Encoding/Utf8.hs \
	# ~/experiments/text/Data/Text/Internal/Encoding/Utf32.hs \
	# ~/experiments/text/Data/Text/Internal/Encoding/Fusion.hs \
	# ~/experiments/text/Data/Text/Encoding.hs \
	# ~/experiments/text/Data/Text.hs \
	# ~/experiments/hashable/Data/Hashable/Class.hs \
	# ~/experiments/hashable/Data/Hashable.hs \
	# /home/mdupont/experiments/universum/src/Universum/Base.hs \
	# /home/mdupont/experiments/universum/src/Universum/Applicative.hs \
	# /home/mdupont/experiments/universum/src/Universum/Container/Reexport.hs \
	# /home/mdupont/experiments/universum/src/Universum/Container/Class.hs \
	# /home/mdupont/experiments/universum/src/Universum/Container.hs \
	# /home/mdupont/experiments/universum/src/Universum/Monad.hs \
	# /home/mdupont/experiments/universum/src/Universum/Function.hs \
	# /home/mdupont/experiments/universum/src/Universum/Bool/Reexport.hs \
	# /home/mdupont/experiments/universum/src/Universum/Bool/Guard.hs \
	# /home/mdupont/experiments/universum/src/Universum/Bool.hs \
	# /home/mdupont/experiments/universum/src/Universum.hs \

#	./Pos/Binary/Class.hs 
#	./Pos/Core/Common/Address.hs

test:
	PATH=/home/mdupont/.local/bin:$(PATH) /home/mdupont/.stack/programs/x86_64-linux/ghc-nopie-8.2.2/bin/ghc \
	-dppr-debug \
	-fno-max-relevant-binds \
	-keep-tmp-files \
	-c  \
	-XLambdaCase \
	-XTypeApplications \
	-XDeriveDataTypeable \
	-XDeriveGeneric \
	-XGeneralizedNewtypeDeriving \
	-XStandaloneDeriving \
	-XFlexibleContexts \
	-XFlexibleInstances \
        -XMultiParamTypeClasses \
                       -XFunctionalDependencies \
                       -XDefaultSignatures \
                       -XNoImplicitPrelude \
                       -XOverloadedStrings \
                       -XRecordWildCards \
                       -XTypeApplications \
                       -XTupleSections \
                       -XViewPatterns \
                       -XLambdaCase \
                       -XMultiWayIf \
                       -XConstraintKinds \
                       -XUndecidableInstances \
                       -XBangPatterns \
                       -XTemplateHaskell \
                       -XScopedTypeVariables \
	./Pos/Data/Attributes.hs \
	./Pos/Core/Constants.hs \
	./Pos/Core/Common/Types.hs \
	./Pos/Core/Common/Coin.hs \
	./Pos/Core/Common.hs \
	./Pos/Core/Class.hs \
	./Pos/Core/Block/Blockchain.hs 
