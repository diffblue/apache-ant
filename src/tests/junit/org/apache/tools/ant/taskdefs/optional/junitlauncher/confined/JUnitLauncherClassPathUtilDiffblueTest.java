package org.apache.tools.ant.taskdefs.optional.junitlauncher.confined;

import static org.junit.Assert.assertFalse;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class JUnitLauncherClassPathUtilDiffblueTest {
  /**
   * Test {@link JUnitLauncherClassPathUtil#addResourceLocationToPath(Path, ClassLoader, String)}.
   * <ul>
   *   <li>When {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitLauncherClassPathUtil#addResourceLocationToPath(Path, ClassLoader, String)}
   */
  @Test
  public void testAddResourceLocationToPath_whenAntClassLoader_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(JUnitLauncherClassPathUtil.addResourceLocationToPath(Path.systemBootClasspath, new AntClassLoader(),
        "Resource"));
  }

  /**
   * Test {@link JUnitLauncherClassPathUtil#addResourceLocationToPath(Path, ClassLoader, String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitLauncherClassPathUtil#addResourceLocationToPath(Path, ClassLoader, String)}
   */
  @Test
  public void testAddResourceLocationToPath_whenEmptyString_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(
        JUnitLauncherClassPathUtil.addResourceLocationToPath(Path.systemBootClasspath, new AntClassLoader(), ""));
  }

  /**
   * Test {@link JUnitLauncherClassPathUtil#addResourceLocationToPath(Path, ClassLoader, String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitLauncherClassPathUtil#addResourceLocationToPath(Path, ClassLoader, String)}
   */
  @Test
  public void testAddResourceLocationToPath_whenNull_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(JUnitLauncherClassPathUtil.addResourceLocationToPath(Path.systemBootClasspath, null, "Resource"));
  }

  /**
   * Test {@link JUnitLauncherClassPathUtil#hasJUnitPlatformResources(ClassLoader)}.
   * <ul>
   *   <li>When {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitLauncherClassPathUtil#hasJUnitPlatformResources(ClassLoader)}
   */
  @Test
  public void testHasJUnitPlatformResources_whenAntClassLoader() {
    // Arrange, Act and Assert
    assertFalse(JUnitLauncherClassPathUtil.hasJUnitPlatformResources(new AntClassLoader()));
  }

  /**
   * Test {@link JUnitLauncherClassPathUtil#hasJUnitPlatformResources(ClassLoader)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JUnitLauncherClassPathUtil#hasJUnitPlatformResources(ClassLoader)}
   */
  @Test
  public void testHasJUnitPlatformResources_whenNull() {
    // Arrange, Act and Assert
    assertFalse(JUnitLauncherClassPathUtil.hasJUnitPlatformResources(null));
  }
}
