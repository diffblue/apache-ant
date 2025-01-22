package org.apache.tools.ant.loader;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class AntClassLoader5DiffblueTest {
  /**
   * Test {@link AntClassLoader5#AntClassLoader5(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then {@link Project} (default constructor) BuildListeners size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader5#AntClassLoader5(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader5_whenAntClassLoader_thenProjectBuildListenersSizeIsTwo() {
    // Arrange
    AntClassLoader parent = new AntClassLoader();

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act
    new AntClassLoader5(parent, project, Path.systemBootClasspath, true);

    // Assert
    assertEquals(2, project.getBuildListeners().size());
  }

  /**
   * Test {@link AntClassLoader5#AntClassLoader5(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader5#AntClassLoader5(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader5_whenNull_thenReturnNotNull() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader5(null, null, null, true));
  }

  /**
   * Test {@link AntClassLoader5#AntClassLoader5(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@code null} and path is {@code *}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader5#AntClassLoader5(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader5_whenPathWithPIsNullAndPathIsAsterisk() {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertNotNull(new AntClassLoader5(null, project, new Path(null, "*"), true));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link AntClassLoader5#AntClassLoader5(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@code null} and path is {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader5#AntClassLoader5(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader5_whenPathWithPIsNullAndPathIsDot() {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertNotNull(new AntClassLoader5(null, project, new Path(null, "."), true));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link AntClassLoader5#AntClassLoader5(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@code null} and path is {@code ignore}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader5#AntClassLoader5(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader5_whenPathWithPIsNullAndPathIsIgnore() {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertNotNull(new AntClassLoader5(null, project, new Path(null, "ignore"), true));
    assertEquals(1, project.getBuildListeners().size());
  }
}
