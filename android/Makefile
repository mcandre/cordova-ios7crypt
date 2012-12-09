DEBUG_APK=bin/IOS7Crypt-debug.apk

HTML=assets/www/index.html
CSS=assets/www/css/index.css
JS=assets/www/js/index.js

$(DEBUG_APK): $(HTML) $(CSS) $(JS)
	ant debug

install: $(DEBUG_APK)
	adb install -r $(DEBUG_APK)

clean:
	-rm -rf bin