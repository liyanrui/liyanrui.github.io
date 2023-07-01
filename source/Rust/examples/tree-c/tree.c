#include <locale.h> /* 为了让 g_print 显示中文 */
#include <glib.h>

typedef struct TreeNode {
        char *data;
        GPtrArray *children;
} TreeNode;

static void print_tree(TreeNode *root) {
        g_print("{data: \"%s\", children: [", root->data);
        for (size_t i = 0; i < root->children->len; i++) {
                TreeNode *child = g_ptr_array_index(root->children, i);
                print_tree(child);
                g_print(", ");
        }
        g_print("]}");
}

static void delete_tree(TreeNode *root) {
        for (size_t i = 0; i < root->children->len; i++) {
                TreeNode *child = g_ptr_array_index(root->children, i);
                delete_tree(child);
        }
        g_ptr_array_free(root->children, TRUE);
        free(root);
}

int main(void) {
        setlocale(LC_ALL, ""); /* 为了让 g_print 显示中文 */
        TreeNode *root = malloc(sizeof(TreeNode));
        root->data = "以下 Rust 程序\n"
                "@ hello world #\n"
                "fn main() {\n"
                "    println!(\"Hello world!\");\n"
                "}\n@\n"
                "可在终端打印「Hello world!」。";
        root->children = g_ptr_array_new();
        /* 第一次分割 */
        GRegex *re = g_regex_new("\n[ \t]*@[ \t\n]*", 0, 0, NULL);
        gchar **v = g_regex_split(re, root->data, 0);
        for (size_t i = 0; v[i]; i++) {
                TreeNode *child = malloc(sizeof(TreeNode));
                child->data = v[i];
                child->children = g_ptr_array_new();
                g_ptr_array_add(root->children, child);
        }
        /* 第二次分割 */
        GRegex *re2 = g_regex_new("[ \t]*#[ \t]*\n", 0, 0, NULL);
        TreeNode *child_2nd = g_ptr_array_index(root->children, 1);
        gchar **v2 = g_regex_split(re2, child_2nd->data, 0);
        for (size_t i = 0; v2[i]; i++) {
                TreeNode *child = malloc(sizeof(TreeNode));
                child->data = v2[i];
                child->children = g_ptr_array_new();
                g_ptr_array_add(child_2nd->children, child);
        }
        print_tree(root);
        g_print("\n");
        delete_tree(root);
        g_strfreev(v);
        g_strfreev(v2);
        g_regex_unref(re);
        g_regex_unref(re2);
        return 0;
}
