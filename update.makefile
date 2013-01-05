update_public: a1_public a2_public a3_public a4_public #a5_public
	git push origin

a1_public:
	git checkout a1 ; git merge --no-edit common

a2_public:
	git checkout a2 ; git merge --no-edit common

a3_public:
	git checkout a3 ; git merge --no-edit common

a4_public:
	git checkout a4 ; git merge --no-edit common


# This assumes that you have done:
#   git remote add framework git@github.iu.edu:p423-523-sp13/framework-public.git
update_private: a1_private a2_private a3_private a4_private #a5_private
	git push origin

a1_private:
	git checkout a1 ; git pull --no-edit framework a1

a2_private:
	git checkout a2 ; git pull --no-edit framework a2

a3_private:
	git checkout a3 ; git pull --no-edit framework a3

a4_private:
	git checkout a4 ; git pull --no-edit framework a3
