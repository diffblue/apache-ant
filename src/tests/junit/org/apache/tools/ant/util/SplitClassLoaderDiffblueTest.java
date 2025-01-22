package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class SplitClassLoaderDiffblueTest {
  /**
   * Test {@link SplitClassLoader#SplitClassLoader(ClassLoader, Path, Project, String[])}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SplitClassLoader#SplitClassLoader(ClassLoader, Path, Project, String[])}
   */
  @Test
  public void testNewSplitClassLoader_whenNull_thenReturnNotNull() {
    // Arrange, Act and Assert
    assertNotNull(new SplitClassLoader(null, null, null, new String[]{"Split Classes"}));
  }

  /**
   * Test {@link SplitClassLoader#SplitClassLoader(ClassLoader, Path, Project, String[])}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SplitClassLoader#SplitClassLoader(ClassLoader, Path, Project, String[])}
   */
  @Test
  public void testNewSplitClassLoader_whenNull_thenReturnNotNull2() {
    // Arrange, Act and Assert
    assertNotNull(new SplitClassLoader(new AntClassLoader(), null, null, new String[]{"Split Classes"}));
  }

  /**
   * Test {@link SplitClassLoader#SplitClassLoader(ClassLoader, Path, Project, String[])}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code ignore}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SplitClassLoader#SplitClassLoader(ClassLoader, Path, Project, String[])}
   */
  @Test
  public void testNewSplitClassLoader_whenPathWithPIsProjectAndPathIsIgnore() {
    // Arrange
    AntClassLoader parent = new AntClassLoader();
    Path path = new Path(new Project(), "ignore");

    Project project = new Project();

    // Act and Assert
    assertNotNull(new SplitClassLoader(parent, path, project, new String[]{"Split Classes"}));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link SplitClassLoader#SplitClassLoader(ClassLoader, Path, Project, String[])}.
   * <ul>
   *   <li>When {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SplitClassLoader#SplitClassLoader(ClassLoader, Path, Project, String[])}
   */
  @Test
  public void testNewSplitClassLoader_whenPathWithProjectIsProject() {
    // Arrange
    AntClassLoader parent = new AntClassLoader();
    Path path = new Path(new Project());
    Project project = new Project();

    // Act and Assert
    assertNotNull(new SplitClassLoader(parent, path, project, new String[]{"Split Classes"}));
    assertEquals(1, project.getBuildListeners().size());
  }
}
