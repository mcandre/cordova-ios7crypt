DEBUG_APK=bin/IOS7Crypt-debug.apk

HTML=assets/www/*.html
CSS=assets/www/css/*.css
JS=assets/www/js/*.js

$(JS): ../ios7crypt.hs # ../random.hs
	fay ../ios7crypt.hs

$(DEBUG_APK): $(HTML) $(CSS) $(JS)
	ant debug

install: $(DEBUG_APK)
	adb install -r $(DEBUG_APK)

clean:
	-rm -rf bin
	-rm -rf ../ios7crypt.js ../random.js