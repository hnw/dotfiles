sdk_path_candidates=(
    "$HOME/Development/adt-bundle-mac-x86_64/sdk"
    "$HOME/Development/adt-bundle-mac-x86_64-20131030/sdk"
)

ndk_path_candidates=(
    "$HOME/Development/android-ndk-r10d"
    "$HOME/Development/android-ndk-r9d"
)

android_toolchain_candidates=(
    "$HOME/Development/arm-android-19-toolchain"
    "$HOME/Development/arm-android-18-toolchain"
)

for sdk_path ($sdk_path_candidates); do
    if [ -d $sdk_path ] ; then
	export ANDROID_SDK_PATH=$sdk_path
	export ANDROID_SDK_ROOT=$ANDROID_SDK_PATH
	export PATH=$PATH:$ANDROID_SDK_PATH/platform-tools:$ANDROID_SDK_PATH/tools
	break
    fi
done

for ndk_path ($ndk_path_candidates); do
    if [ -d $ndk_path ] ; then
	export ANDROID_NDK_PATH=$ndk_path
	export ANDROID_NDK_ROOT=$ANDROID_NDK_PATH
	export PATH=$PATH:$ANDROID_NDK_PATH
	break
    fi
done

for toolchain_path ($android_toolchain_candidates); do
    if [ -d $toolchain_path ] ; then
	export ANDROID_TOOKCHAIN_ROOT=$toolchain_path
	export PATH=$PATH:$ANDROID_TOOKCHAIN_ROOT/bin
	break
    fi
done

export ANDROID_APP_ROOT="$HOME/Development/arm-android-18-app"
