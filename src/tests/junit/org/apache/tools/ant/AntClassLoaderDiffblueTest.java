package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Vector;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class AntClassLoaderDiffblueTest {
  /**
   * Test {@link AntClassLoader#AntClassLoader()}.
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader()}
   */
  @Test
  public void testNewAntClassLoader() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader());
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_givenAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNotNull(new AntClassLoader(null, null, new Path(project), true));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(Project, Path)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(Project, Path)}
   */
  @Test
  public void testNewAntClassLoader_givenAntClassLoader_whenPathWithProjectIsProject() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNotNull(new AntClassLoader(null, new Path(project)));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(Project, Path, boolean)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_givenAntClassLoader_whenPathWithProjectIsProject2() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNotNull(new AntClassLoader(null, new Path(project), true));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>Given {@link MagicNames#BUILD_SYSCLASSPATH}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_givenBuild_sysclasspath() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget(MagicNames.BUILD_SYSCLASSPATH, new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNotNull(new AntClassLoader(null, null, new Path(project), true));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(Project, Path)}.
   * <ul>
   *   <li>Given {@link FileList#FileList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(Project, Path)}
   */
  @Test
  public void testNewAntClassLoader_givenFileList() throws BuildException {
    // Arrange
    Path classpath = new Path(new Project());
    classpath.addFilelist(new FileList());

    // Act and Assert
    assertNotNull(new AntClassLoader(null, classpath));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(Project, Path, boolean)}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_givenTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget(MagicNames.BUILD_SYSCLASSPATH, new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNotNull(new AntClassLoader(null, new Path(project), true));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(ClassLoader, boolean)}.
   * <ul>
   *   <li>When {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(ClassLoader, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenAntClassLoader() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader(new AntClassLoader(), true));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenAntClassLoader_thenReturnNotNull() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader(new AntClassLoader(), null, null, true));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(ClassLoader, boolean)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(ClassLoader, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenNull() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader(null, true));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path)}
   */
  @Test
  public void testNewAntClassLoader_whenNull_thenReturnNotNull() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader(null, null, null));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenNull_thenReturnNotNull2() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader(null, null, null, true));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(Project, Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(Project, Path)}
   */
  @Test
  public void testNewAntClassLoader_whenNull_thenReturnNotNull3() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader(null, null));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(Project, Path, boolean)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenNull_thenReturnNotNull4() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader(null, null, true));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@code null} and path is {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithPIsNullAndPathIsDot() {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertNotNull(new AntClassLoader(null, project, new Path(null, "."), true));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@code null} and {@code Path}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithPIsNullAndPath_thenReturnNotNull() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader(null, null, new Path(null, "Path")));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(Project, Path)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@code null} and {@code Path}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(Project, Path)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithPIsNullAndPath_thenReturnNotNull2() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader(null, new Path(null, "Path")));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code *}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithPIsProjectAndPathIsAsterisk() {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertNotNull(new AntClassLoader(null, project, new Path(new Project(), "*"), true));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(Project, Path)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code *}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(Project, Path)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithPIsProjectAndPathIsAsterisk_thenReturnNotNull() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader(null, new Path(new Project(), "*")));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithPIsProjectAndPathIsDot() {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertNotNull(new AntClassLoader(null, project, new Path(new Project(), "."), true));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code .}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithPIsProjectAndPathIsDot_thenReturnNotNull() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader(null, null, new Path(new Project(), "."), true));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(Project, Path)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code .}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(Project, Path)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithPIsProjectAndPathIsDot_thenReturnNotNull2() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader(null, new Path(new Project(), ".")));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code .}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithPIsProjectAndPathIsDot_thenReturnNotNull3() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader(null, new Path(new Project(), "."), true));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code ignore}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithPIsProjectAndPathIsIgnore() {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertNotNull(new AntClassLoader(null, project, new Path(new Project(), "ignore"), true));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(Project, Path)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and {@code Path}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(Project, Path)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithPIsProjectAndPath_thenReturnNotNull() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader(null, new Path(new Project(), "Path")));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project)} with project is {@code null}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithProjectIsNull_thenReturnNotNull() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader(null, null, new Path(null), true));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(Project, Path)}.
   * <ul>
   *   <li>When {@link Path#Path(Project)} with project is {@code null}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(Project, Path)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithProjectIsNull_thenReturnNotNull2() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader(null, new Path(null)));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project)} with project is {@code null}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithProjectIsNull_thenReturnNotNull3() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader(null, new Path(null), true));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithProjectIsProject_thenReturnNotNull() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader(null, null, new Path(new Project()), true));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(Project, Path)}.
   * <ul>
   *   <li>When {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(Project, Path)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithProjectIsProject_thenReturnNotNull2() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader(null, new Path(new Project())));
  }

  /**
   * Test {@link AntClassLoader#AntClassLoader(Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#AntClassLoader(Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithProjectIsProject_thenReturnNotNull3() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader(null, new Path(new Project()), true));
  }

  /**
   * Test {@link AntClassLoader#setProject(Project)}.
   * <ul>
   *   <li>Then {@link Project} (default constructor) BuildListeners size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#setProject(Project)}
   */
  @Test
  public void testSetProject_thenProjectBuildListenersSizeIsTwo() {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act
    antClassLoader.setProject(project);

    // Assert
    Vector<BuildListener> buildListeners = project.getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(antClassLoader, buildListeners.get(1));
  }

  /**
   * Test {@link AntClassLoader#setProject(Project)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then {@link Project} (default constructor) BuildListeners size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#setProject(Project)}
   */
  @Test
  public void testSetProject_whenProject_thenProjectBuildListenersSizeIsOne() {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    Project project = new Project();

    // Act
    antClassLoader.setProject(project);

    // Assert
    Vector<BuildListener> buildListeners = project.getBuildListeners();
    assertEquals(1, buildListeners.size());
    assertSame(antClassLoader, buildListeners.get(0));
  }

  /**
   * Test {@link AntClassLoader#getClasspath()}.
   * <p>
   * Method under test: {@link AntClassLoader#getClasspath()}
   */
  @Test
  public void testGetClasspath() {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    String actualClasspath = antClassLoader.getClasspath();

    // Assert
    assertEquals(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(), actualClasspath);
  }

  /**
   * Test {@link AntClassLoader#getClasspath()}.
   * <p>
   * Method under test: {@link AntClassLoader#getClasspath()}
   */
  @Test
  public void testGetClasspath2() throws BuildException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.addPathElement(".");
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    String actualClasspath = antClassLoader.getClasspath();

    // Assert
    String toStringResult = Paths.get(System.getProperty("user.dir"), ".").toString();
    assertEquals(
        String.join("", toStringResult, ":", Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString()),
        actualClasspath);
  }

  /**
   * Test {@link AntClassLoader#getClasspath()}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#getClasspath()}
   */
  @Test
  public void testGetClasspath_givenAntClassLoader_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new AntClassLoader()).getClasspath());
  }

  /**
   * Test {@link AntClassLoader#forceLoadClass(String)}.
   * <p>
   * Method under test: {@link AntClassLoader#forceLoadClass(String)}
   */
  @Test
  public void testForceLoadClass() throws ClassNotFoundException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(ClassNotFoundException.class, () -> antClassLoader.forceLoadClass("Classname"));
  }

  /**
   * Test {@link AntClassLoader#forceLoadClass(String)}.
   * <p>
   * Method under test: {@link AntClassLoader#forceLoadClass(String)}
   */
  @Test
  public void testForceLoadClass2() throws ClassNotFoundException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "force loading ").toFile());

    // Act and Assert
    assertThrows(ClassNotFoundException.class, () -> antClassLoader.forceLoadClass("Classname"));
  }

  /**
   * Test {@link AntClassLoader#forceLoadClass(String)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#forceLoadClass(String)}
   */
  @Test
  public void testForceLoadClass_givenAntClassLoaderProjectIsProject() throws ClassNotFoundException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.setProject(new Project());
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(ClassNotFoundException.class, () -> antClassLoader.forceLoadClass("Classname"));
  }

  /**
   * Test {@link AntClassLoader#forceLoadClass(String)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link ClassNotFoundException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#forceLoadClass(String)}
   */
  @Test
  public void testForceLoadClass_givenAntClassLoader_thenThrowClassNotFoundException() throws ClassNotFoundException {
    // Arrange, Act and Assert
    assertThrows(ClassNotFoundException.class, () -> (new AntClassLoader()).forceLoadClass("Classname"));
  }

  /**
   * Test {@link AntClassLoader#forceLoadSystemClass(String)}.
   * <p>
   * Method under test: {@link AntClassLoader#forceLoadSystemClass(String)}
   */
  @Test
  public void testForceLoadSystemClass() throws ClassNotFoundException {
    // Arrange
    AntClassLoader parent = new AntClassLoader();
    parent.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.setParent(parent);
    antClassLoader.setProject(null);

    // Act and Assert
    assertThrows(ClassNotFoundException.class, () -> antClassLoader.forceLoadSystemClass("Classname"));
  }

  /**
   * Test {@link AntClassLoader#forceLoadSystemClass(String)}.
   * <p>
   * Method under test: {@link AntClassLoader#forceLoadSystemClass(String)}
   */
  @Test
  public void testForceLoadSystemClass2() throws ClassNotFoundException {
    // Arrange
    AntClassLoader parent = new AntClassLoader();
    parent.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "force system loading ").toFile());

    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.setParent(parent);
    antClassLoader.setProject(null);

    // Act and Assert
    assertThrows(ClassNotFoundException.class, () -> antClassLoader.forceLoadSystemClass("Classname"));
  }

  /**
   * Test {@link AntClassLoader#forceLoadSystemClass(String)}.
   * <p>
   * Method under test: {@link AntClassLoader#forceLoadSystemClass(String)}
   */
  @Test
  public void testForceLoadSystemClass3() throws ClassNotFoundException {
    // Arrange and Act
    Class<?> actualForceLoadSystemClassResult = (new AntClassLoader(new AntClassLoader(), true))
        .forceLoadSystemClass("java.io.File");

    // Assert
    Class<File> expectedForceLoadSystemClassResult = File.class;
    assertEquals(expectedForceLoadSystemClassResult, actualForceLoadSystemClassResult);
  }

  /**
   * Test {@link AntClassLoader#forceLoadSystemClass(String)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()} addJavaLibraries.</li>
   *   <li>Then return {@link File}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#forceLoadSystemClass(String)}
   */
  @Test
  public void testForceLoadSystemClass_givenAntClassLoaderAddJavaLibraries_thenReturnFile()
      throws ClassNotFoundException {
    // Arrange
    AntClassLoader parent = new AntClassLoader();
    parent.addJavaLibraries();

    // Act
    Class<?> actualForceLoadSystemClassResult = (new AntClassLoader(parent, true)).forceLoadSystemClass("java.io.File");

    // Assert
    Class<File> expectedForceLoadSystemClassResult = File.class;
    assertEquals(expectedForceLoadSystemClassResult, actualForceLoadSystemClassResult);
  }

  /**
   * Test {@link AntClassLoader#forceLoadSystemClass(String)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@code File}.</li>
   *   <li>Then return {@link File}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#forceLoadSystemClass(String)}
   */
  @Test
  public void testForceLoadSystemClass_givenAntClassLoader_whenJavaIoFile_thenReturnFile()
      throws ClassNotFoundException {
    // Arrange and Act
    Class<?> actualForceLoadSystemClassResult = (new AntClassLoader()).forceLoadSystemClass("java.io.File");

    // Assert
    Class<File> expectedForceLoadSystemClassResult = File.class;
    assertEquals(expectedForceLoadSystemClassResult, actualForceLoadSystemClassResult);
  }

  /**
   * Test {@link AntClassLoader#forceLoadSystemClass(String)}.
   * <ul>
   *   <li>Then throw {@link ClassNotFoundException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#forceLoadSystemClass(String)}
   */
  @Test
  public void testForceLoadSystemClass_thenThrowClassNotFoundException() throws ClassNotFoundException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.setParent(new AntClassLoader());
    antClassLoader.setProject(null);

    // Act and Assert
    assertThrows(ClassNotFoundException.class, () -> antClassLoader.forceLoadSystemClass("Classname"));
  }

  /**
   * Test {@link AntClassLoader#getResourceAsStream(String)}.
   * <p>
   * Method under test: {@link AntClassLoader#getResourceAsStream(String)}
   */
  @Test
  public void testGetResourceAsStream() {
    // Arrange, Act and Assert
    assertNull((new AntClassLoader(new AntClassLoader(), true)).getResourceAsStream("Name"));
  }

  /**
   * Test {@link AntClassLoader#getResourceAsStream(String)}.
   * <p>
   * Method under test: {@link AntClassLoader#getResourceAsStream(String)}
   */
  @Test
  public void testGetResourceAsStream2() {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertNull(antClassLoader.getResourceAsStream("Name"));
  }

  /**
   * Test {@link AntClassLoader#getResourceAsStream(String)}.
   * <p>
   * Method under test: {@link AntClassLoader#getResourceAsStream(String)}
   */
  @Test
  public void testGetResourceAsStream3() {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.addPathComponent(
        Paths.get(System.getProperty("java.io.tmpdir"), "Couldn't load ResourceStream for ").toFile());

    // Act and Assert
    assertNull(antClassLoader.getResourceAsStream("Name"));
  }

  /**
   * Test {@link AntClassLoader#getResourceAsStream(String)}.
   * <p>
   * Method under test: {@link AntClassLoader#getResourceAsStream(String)}
   */
  @Test
  public void testGetResourceAsStream4() {
    // Arrange, Act and Assert
    assertNull((new AntClassLoader(new AntClassLoader(), false)).getResourceAsStream("Name"));
  }

  /**
   * Test {@link AntClassLoader#getResourceAsStream(String)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()} addPathElement {@code .}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#getResourceAsStream(String)}
   */
  @Test
  public void testGetResourceAsStream_givenAntClassLoaderAddPathElementDot_thenReturnNull() throws BuildException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.addPathElement(".");
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertNull(antClassLoader.getResourceAsStream("Name"));
  }

  /**
   * Test {@link AntClassLoader#getResourceAsStream(String)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#getResourceAsStream(String)}
   */
  @Test
  public void testGetResourceAsStream_givenAntClassLoaderProjectIsProject_thenReturnNull() {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.setProject(new Project());
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertNull(antClassLoader.getResourceAsStream("Name"));
  }

  /**
   * Test {@link AntClassLoader#getResourceAsStream(String)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@code Name}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#getResourceAsStream(String)}
   */
  @Test
  public void testGetResourceAsStream_givenAntClassLoader_whenName_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new AntClassLoader()).getResourceAsStream("Name"));
  }

  /**
   * Test {@link AntClassLoader#getResourceAsStream(String)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@code Name}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#getResourceAsStream(String)}
   */
  @Test
  public void testGetResourceAsStream_givenJavaLangObject_whenName_thenReturnNull() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Couldn't load ResourceStream for ", typeClass);

    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.setProject(project);
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertNull(antClassLoader.getResourceAsStream("Name"));
  }

  /**
   * Test {@link AntClassLoader#getResource(String)}.
   * <p>
   * Method under test: {@link AntClassLoader#getResource(String)}
   */
  @Test
  public void testGetResource() {
    // Arrange, Act and Assert
    assertNull((new AntClassLoader(new AntClassLoader(), true)).getResource("Name"));
  }

  /**
   * Test {@link AntClassLoader#getResource(String)}.
   * <p>
   * Method under test: {@link AntClassLoader#getResource(String)}
   */
  @Test
  public void testGetResource2() {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertNull(antClassLoader.getResource("Name"));
  }

  /**
   * Test {@link AntClassLoader#getResource(String)}.
   * <p>
   * Method under test: {@link AntClassLoader#getResource(String)}
   */
  @Test
  public void testGetResource3() {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader
        .addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "Couldn't load Resource ").toFile());

    // Act and Assert
    assertNull(antClassLoader.getResource("Name"));
  }

  /**
   * Test {@link AntClassLoader#getResource(String)}.
   * <p>
   * Method under test: {@link AntClassLoader#getResource(String)}
   */
  @Test
  public void testGetResource4() {
    // Arrange, Act and Assert
    assertNull((new AntClassLoader(new AntClassLoader(), false)).getResource("Name"));
  }

  /**
   * Test {@link AntClassLoader#getResource(String)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()} Project is {@link Project} (default constructor).</li>
   *   <li>When {@code Name}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#getResource(String)}
   */
  @Test
  public void testGetResource_givenAntClassLoaderProjectIsProject_whenName_thenReturnNull() {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.setProject(new Project());
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertNull(antClassLoader.getResource("Name"));
  }

  /**
   * Test {@link AntClassLoader#getResource(String)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@code Name}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#getResource(String)}
   */
  @Test
  public void testGetResource_givenAntClassLoader_whenName_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new AntClassLoader()).getResource("Name"));
  }

  /**
   * Test {@link AntClassLoader#getResource(String)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@code Name}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#getResource(String)}
   */
  @Test
  public void testGetResource_givenJavaLangObject_whenName_thenReturnNull() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Couldn't load Resource ", typeClass);

    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.setProject(project);
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertNull(antClassLoader.getResource("Name"));
  }

  /**
   * Test {@link AntClassLoader#findResource(String)} with {@code String}.
   * <p>
   * Method under test: {@link AntClassLoader#findResource(String)}
   */
  @Test
  public void testFindResourceWithString() {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "Resource ").toFile());

    // Act and Assert
    assertNull(antClassLoader.findResource("."));
  }

  /**
   * Test {@link AntClassLoader#findResource(String)} with {@code String}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()} Project is {@link Project} (default constructor).</li>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#findResource(String)}
   */
  @Test
  public void testFindResourceWithString_givenAntClassLoaderProjectIsProject_whenNull() {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.setProject(new Project());
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertNull(antClassLoader.findResource(null));
  }

  /**
   * Test {@link AntClassLoader#findResource(String)} with {@code String}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@code Name}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#findResource(String)}
   */
  @Test
  public void testFindResourceWithString_givenAntClassLoader_whenName_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new AntClassLoader()).findResource("Name"));
  }

  /**
   * Test {@link AntClassLoader#findResource(String)} with {@code String}.
   * <ul>
   *   <li>When {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#findResource(String)}
   */
  @Test
  public void testFindResourceWithString_whenName() {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertNull(antClassLoader.findResource("Name"));
  }

  /**
   * Test {@link AntClassLoader#findResource(String)} with {@code String}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#findResource(String)}
   */
  @Test
  public void testFindResourceWithString_whenNull() {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertNull(antClassLoader.findResource(null));
  }

  /**
   * Test {@link AntClassLoader#getResourceURL(File, String)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()} Project is {@link Project} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#getResourceURL(File, String)}
   */
  @Test
  public void testGetResourceURL_givenAntClassLoaderProjectIsProject_whenNull_thenReturnNull() {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.setProject(new Project());

    // Act and Assert
    assertNull(
        antClassLoader.getResourceURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), null));
  }

  /**
   * Test {@link AntClassLoader#getResourceURL(File, String)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@code https://example.org/example}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#getResourceURL(File, String)}
   */
  @Test
  public void testGetResourceURL_givenAntClassLoader_whenHttpsExampleOrgExample_thenReturnNull() {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();

    // Act and Assert
    assertNull(antClassLoader.getResourceURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        "https://example.org/example"));
  }

  /**
   * Test {@link AntClassLoader#getResourceURL(File, String)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#getResourceURL(File, String)}
   */
  @Test
  public void testGetResourceURL_givenAntClassLoader_whenNull_thenReturnNull() {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();

    // Act and Assert
    assertNull(
        antClassLoader.getResourceURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), null));
  }

  /**
   * Test {@link AntClassLoader#loadClass(String, boolean)} with {@code String}, {@code boolean}.
   * <p>
   * Method under test: {@link AntClassLoader#loadClass(String, boolean)}
   */
  @Test
  public void testLoadClassWithStringBoolean() throws ClassNotFoundException {
    // Arrange, Act and Assert
    assertThrows(ClassNotFoundException.class,
        () -> (new AntClassLoader(new AntClassLoader(), true)).loadClass("Classname", true));
  }

  /**
   * Test {@link AntClassLoader#loadClass(String, boolean)} with {@code String}, {@code boolean}.
   * <p>
   * Method under test: {@link AntClassLoader#loadClass(String, boolean)}
   */
  @Test
  public void testLoadClassWithStringBoolean2() throws ClassNotFoundException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(ClassNotFoundException.class, () -> antClassLoader.loadClass("Classname", true));
  }

  /**
   * Test {@link AntClassLoader#loadClass(String, boolean)} with {@code String}, {@code boolean}.
   * <p>
   * Method under test: {@link AntClassLoader#loadClass(String, boolean)}
   */
  @Test
  public void testLoadClassWithStringBoolean3() throws ClassNotFoundException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "Finding class ").toFile());

    // Act and Assert
    assertThrows(ClassNotFoundException.class, () -> antClassLoader.loadClass("Classname", true));
  }

  /**
   * Test {@link AntClassLoader#loadClass(String, boolean)} with {@code String}, {@code boolean}.
   * <p>
   * Method under test: {@link AntClassLoader#loadClass(String, boolean)}
   */
  @Test
  public void testLoadClassWithStringBoolean4() throws ClassNotFoundException {
    // Arrange, Act and Assert
    assertThrows(ClassNotFoundException.class,
        () -> (new AntClassLoader(new AntClassLoader(), false)).loadClass("Classname", true));
  }

  /**
   * Test {@link AntClassLoader#loadClass(String, boolean)} with {@code String}, {@code boolean}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#loadClass(String, boolean)}
   */
  @Test
  public void testLoadClassWithStringBoolean_givenAntClassLoader() throws ClassNotFoundException {
    // Arrange, Act and Assert
    assertThrows(ClassNotFoundException.class, () -> (new AntClassLoader()).loadClass("Classname", true));
  }

  /**
   * Test {@link AntClassLoader#loadClass(String, boolean)} with {@code String}, {@code boolean}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#loadClass(String, boolean)}
   */
  @Test
  public void testLoadClassWithStringBoolean_givenAntClassLoaderProjectIsProject() throws ClassNotFoundException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.setProject(new Project());
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(ClassNotFoundException.class, () -> antClassLoader.loadClass("Classname", true));
  }

  /**
   * Test {@link AntClassLoader#loadClass(String, boolean)} with {@code String}, {@code boolean}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#loadClass(String, boolean)}
   */
  @Test
  public void testLoadClassWithStringBoolean_givenJavaLangObject() throws ClassNotFoundException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Finding class ", typeClass);

    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.setProject(project);
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(ClassNotFoundException.class, () -> antClassLoader.loadClass("Classname", true));
  }

  /**
   * Test {@link AntClassLoader#findClass(String)} with {@code String}.
   * <p>
   * Method under test: {@link AntClassLoader#findClass(String)}
   */
  @Test
  public void testFindClassWithString() throws ClassNotFoundException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(ClassNotFoundException.class, () -> antClassLoader.findClass("Name"));
  }

  /**
   * Test {@link AntClassLoader#findClass(String)} with {@code String}.
   * <p>
   * Method under test: {@link AntClassLoader#findClass(String)}
   */
  @Test
  public void testFindClassWithString2() throws ClassNotFoundException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "Finding class ").toFile());

    // Act and Assert
    assertThrows(ClassNotFoundException.class, () -> antClassLoader.findClass("Name"));
  }

  /**
   * Test {@link AntClassLoader#findClass(String)} with {@code String}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#findClass(String)}
   */
  @Test
  public void testFindClassWithString_givenAntClassLoaderProjectIsProject() throws ClassNotFoundException {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.setProject(new Project());
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(ClassNotFoundException.class, () -> antClassLoader.findClass("Name"));
  }

  /**
   * Test {@link AntClassLoader#findClass(String)} with {@code String}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link ClassNotFoundException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#findClass(String)}
   */
  @Test
  public void testFindClassWithString_givenAntClassLoader_thenThrowClassNotFoundException()
      throws ClassNotFoundException {
    // Arrange, Act and Assert
    assertThrows(ClassNotFoundException.class, () -> (new AntClassLoader()).findClass("Name"));
  }

  /**
   * Test {@link AntClassLoader#findClass(String)} with {@code String}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link ClassNotFoundException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#findClass(String)}
   */
  @Test
  public void testFindClassWithString_givenJavaLangObject_thenThrowClassNotFoundException()
      throws ClassNotFoundException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Finding class ", typeClass);

    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.setProject(project);
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(ClassNotFoundException.class, () -> antClassLoader.findClass("Name"));
  }

  /**
   * Test {@link AntClassLoader#isInPath(File)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#isInPath(File)}
   */
  @Test
  public void testIsInPath_givenAntClassLoader_thenReturnFalse() {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();

    // Act and Assert
    assertFalse(antClassLoader.isInPath(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link AntClassLoader#isInPath(File)}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#isInPath(File)}
   */
  @Test
  public void testIsInPath_thenReturnTrue() {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();
    antClassLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertTrue(antClassLoader.isInPath(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link AntClassLoader#setIsolated(boolean)}
   *   <li>{@link AntClassLoader#setParentFirst(boolean)}
   *   <li>{@link AntClassLoader#buildStarted(BuildEvent)}
   *   <li>{@link AntClassLoader#messageLogged(BuildEvent)}
   *   <li>{@link AntClassLoader#subBuildStarted(BuildEvent)}
   *   <li>{@link AntClassLoader#targetFinished(BuildEvent)}
   *   <li>{@link AntClassLoader#targetStarted(BuildEvent)}
   *   <li>{@link AntClassLoader#taskFinished(BuildEvent)}
   *   <li>{@link AntClassLoader#taskStarted(BuildEvent)}
   *   <li>{@link AntClassLoader#toString()}
   *   <li>{@link AntClassLoader#getConfiguredParent()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    AntClassLoader antClassLoader = new AntClassLoader();

    // Act
    antClassLoader.setIsolated(true);
    antClassLoader.setParentFirst(true);
    antClassLoader.buildStarted(new BuildEvent(new Project()));
    antClassLoader.messageLogged(new BuildEvent(new Project()));
    antClassLoader.subBuildStarted(new BuildEvent(new Project()));
    antClassLoader.targetFinished(new BuildEvent(new Project()));
    antClassLoader.targetStarted(new BuildEvent(new Project()));
    antClassLoader.taskFinished(new BuildEvent(new Project()));
    antClassLoader.taskStarted(new BuildEvent(new Project()));
    String actualToStringResult = antClassLoader.toString();
    antClassLoader.getConfiguredParent();

    // Assert
    assertEquals("AntClassLoader[]", actualToStringResult);
  }

  /**
   * Test {@link AntClassLoader#newAntClassLoader(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#newAntClassLoader(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_givenAntClassLoader2() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNotNull(AntClassLoader.newAntClassLoader(null, null, new Path(project), true));
  }

  /**
   * Test {@link AntClassLoader#newAntClassLoader(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>Given {@link MagicNames#BUILD_SYSCLASSPATH}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#newAntClassLoader(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_givenBuild_sysclasspath2() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget(MagicNames.BUILD_SYSCLASSPATH, new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNotNull(AntClassLoader.newAntClassLoader(null, null, new Path(project), true));
  }

  /**
   * Test {@link AntClassLoader#newAntClassLoader(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#newAntClassLoader(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenAntClassLoader_thenReturnNotNull2() {
    // Arrange, Act and Assert
    assertNotNull(AntClassLoader.newAntClassLoader(new AntClassLoader(), null, null, true));
  }

  /**
   * Test {@link AntClassLoader#newAntClassLoader(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#newAntClassLoader(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenNull_thenReturnNotNull5() {
    // Arrange, Act and Assert
    assertNotNull(AntClassLoader.newAntClassLoader(null, null, null, true));
  }

  /**
   * Test {@link AntClassLoader#newAntClassLoader(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@code null} and path is {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#newAntClassLoader(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithPIsNullAndPathIsDot2() {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertNotNull(AntClassLoader.newAntClassLoader(null, project, new Path(null, "."), true));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link AntClassLoader#newAntClassLoader(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code *}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#newAntClassLoader(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithPIsProjectAndPathIsAsterisk2() {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertNotNull(AntClassLoader.newAntClassLoader(null, project, new Path(new Project(), "*"), true));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link AntClassLoader#newAntClassLoader(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#newAntClassLoader(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithPIsProjectAndPathIsDot2() {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertNotNull(AntClassLoader.newAntClassLoader(null, project, new Path(new Project(), "."), true));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link AntClassLoader#newAntClassLoader(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code .}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#newAntClassLoader(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithPIsProjectAndPathIsDot_thenReturnNotNull4() {
    // Arrange, Act and Assert
    assertNotNull(AntClassLoader.newAntClassLoader(null, null, new Path(new Project(), "."), true));
  }

  /**
   * Test {@link AntClassLoader#newAntClassLoader(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code ignore}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#newAntClassLoader(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithPIsProjectAndPathIsIgnore2() {
    // Arrange
    Project project = new Project();

    // Act and Assert
    assertNotNull(AntClassLoader.newAntClassLoader(null, project, new Path(new Project(), "ignore"), true));
    assertEquals(1, project.getBuildListeners().size());
  }

  /**
   * Test {@link AntClassLoader#newAntClassLoader(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project)} with project is {@code null}.</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#newAntClassLoader(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithProjectIsNull_thenReturnNotNull4() {
    // Arrange, Act and Assert
    assertNotNull(AntClassLoader.newAntClassLoader(null, null, new Path(null), true));
  }

  /**
   * Test {@link AntClassLoader#newAntClassLoader(ClassLoader, Project, Path, boolean)}.
   * <ul>
   *   <li>When {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   *   <li>Then return not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntClassLoader#newAntClassLoader(ClassLoader, Project, Path, boolean)}
   */
  @Test
  public void testNewAntClassLoader_whenPathWithProjectIsProject_thenReturnNotNull4() {
    // Arrange, Act and Assert
    assertNotNull(AntClassLoader.newAntClassLoader(null, null, new Path(new Project()), true));
  }
}
