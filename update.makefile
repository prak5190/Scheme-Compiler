update: a1 a2
	git push origin

a%:
	git checkout $@
ifneq (,$(findstring public,$(notdir $(CURDIR))))
	git merge --no-edit common
else
ifneq (,$(findstring private,$(notdir $(CURDIR))))
	git pull --no-edit framework $@
else
endif
endif

#a2_public:
#	git checkout a2 ; git merge --no-edit common

#update_private: a1_private a2_private #a3_private a4_private a5_private
#	git push origin
#
#a1_private:
#	git checkout a1 ; git pull --no-edit framework a1
#
#a2_private:
#	git checkout a2 ; git pull --no-edit framework a2
