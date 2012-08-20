update_public: a1_public a2_public #a3_public a4_public a5_public
	git push origin

a1_public:
	git checkout a1 ; git merge --no-edit common

a2_public:
	git checkout a2 ; git merge --no-edit common

#a3_public:
#	cd public ; git checkout a3 ; git merge --no-edit common ; git push origin a3
#
#a4_public:
#	cd public ; git checkout a4 ; git merge --no-edit common ; git push origin a4
#
#a5_public:
#	cd public ; git checkout a5 ; git merge --no-edit common ; git push origin a5

update_private: a1_private a2_private #a3_private a4_private a5_private
	git push origin

a1_private:
	git checkout a1 ; git pull --no-edit framework a1

a2_private:
	git checkout a2 ; git pull --no-edit framework a2

#a3_private:
#	cd private ; git checkout a3 ; git pull --no-edit framework a3 ; git push origin a3
#
#a4_private:
#	cd private ; git checkout a4 ; git pull --no-edit framework a4 ; git push origin a4
#
#a5_private:
#	cd private ; git checkout a5 ; git pull --no-edit framework a5 ; git push origin a5
