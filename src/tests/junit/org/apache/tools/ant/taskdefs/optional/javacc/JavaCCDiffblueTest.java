package org.apache.tools.ant.taskdefs.optional.javacc;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileList.FileName;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class JavaCCDiffblueTest {
  /**
   * Test new {@link JavaCC} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link JavaCC}
   */
  @Test
  public void testNewJavaCC() {
    // Arrange and Act
    JavaCC actualJavaCC = new JavaCC();

    // Assert
    Location location = actualJavaCC.getLocation();
    assertNull(location.getFileName());
    assertNull(actualJavaCC.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualJavaCC.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualJavaCC.getTaskName());
    assertNull(actualJavaCC.getTaskType());
    assertNull(actualJavaCC.getProject());
    assertNull(actualJavaCC.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualJavaCC, runtimeConfigurableWrapper.getProxy());
  }

  /**
   * Test {@link JavaCC#execute()}.
   * <p>
   * Method under test: {@link JavaCC#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    JavaCC javaCC = new JavaCC();
    javaCC.setTarget(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> javaCC.execute());
  }

  /**
   * Test {@link JavaCC#execute()}.
   * <ul>
   *   <li>Given {@link JavaCC} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaCC#execute()}
   */
  @Test
  public void testExecute_givenJavaCC() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new JavaCC()).execute());
  }

  /**
   * Test {@link JavaCC#getArchiveFile(File)}.
   * <p>
   * Method under test: {@link JavaCC#getArchiveFile(File)}
   */
  @Test
  public void testGetArchiveFile() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class,
        () -> JavaCC.getArchiveFile(Paths
            .get(System.getProperty("java.io.tmpdir"), "Could not find a path to JavaCC.zip or javacc.jar from '%s'.")
            .toFile()));
  }

  /**
   * Test {@link JavaCC#getArchiveFile(File)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaCC#getArchiveFile(File)}
   */
  @Test
  public void testGetArchiveFile_whenNull() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> JavaCC.getArchiveFile(null));
  }

  /**
   * Test {@link JavaCC#getArchiveFile(File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaCC#getArchiveFile(File)}
   */
  @Test
  public void testGetArchiveFile_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class,
        () -> JavaCC.getArchiveFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link JavaCC#getMainClass(File, int)} with {@code home}, {@code type}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaCC#getMainClass(File, int)}
   */
  @Test
  public void testGetMainClassWithHomeType_whenNull() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> JavaCC.getMainClass((File) null, 1));
  }

  /**
   * Test {@link JavaCC#getMainClass(File, int)} with {@code home}, {@code type}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code getProject} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaCC#getMainClass(File, int)}
   */
  @Test
  public void testGetMainClassWithHomeType_whenPropertyIsJavaIoTmpdirIsGetProjectToFile() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class,
        () -> JavaCC.getMainClass(Paths.get(System.getProperty("java.io.tmpdir"), "getProject").toFile(), 1));
  }

  /**
   * Test {@link JavaCC#getMainClass(File, int)} with {@code home}, {@code type}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaCC#getMainClass(File, int)}
   */
  @Test
  public void testGetMainClassWithHomeType_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class,
        () -> JavaCC.getMainClass(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), 1));
  }

  /**
   * Test {@link JavaCC#getMainClass(Path, int)} with {@code path}, {@code type}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaCC#getMainClass(Path, int)}
   */
  @Test
  public void testGetMainClassWithPathType_givenAntClassLoader() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> JavaCC.getMainClass(new Path(project), 1));
  }

  /**
   * Test {@link JavaCC#getMainClass(Path, int)} with {@code path}, {@code type}.
   * <ul>
   *   <li>Given {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaCC#getMainClass(Path, int)}
   */
  @Test
  public void testGetMainClassWithPathType_givenDefaultLogger() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    // Act and Assert
    assertThrows(BuildException.class, () -> JavaCC.getMainClass(new Path(project), 1));
  }

  /**
   * Test {@link JavaCC#getMainClass(Path, int)} with {@code path}, {@code type}.
   * <ul>
   *   <li>Given {@link FileList#FileList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaCC#getMainClass(Path, int)}
   */
  @Test
  public void testGetMainClassWithPathType_givenFileList() throws BuildException {
    // Arrange
    Path path = new Path(new Project());
    path.addFilelist(new FileList());

    // Act and Assert
    assertThrows(BuildException.class, () -> JavaCC.getMainClass(path, 1));
  }

  /**
   * Test {@link JavaCC#getMainClass(Path, int)} with {@code path}, {@code type}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code Adding reference:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaCC#getMainClass(Path, int)}
   */
  @Test
  public void testGetMainClassWithPathType_givenFileNameNameIsAddingReference() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("Adding reference: ");

    FileList fl = new FileList();
    fl.addConfiguredFile(name);

    Path path = new Path(new Project());
    path.addFilelist(fl);

    // Act and Assert
    assertThrows(BuildException.class, () -> JavaCC.getMainClass(path, 1));
  }

  /**
   * Test {@link JavaCC#getMainClass(Path, int)} with {@code path}, {@code type}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaCC#getMainClass(Path, int)}
   */
  @Test
  public void testGetMainClassWithPathType_givenFileNameNameIsEmptyString() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("");

    FileList fl = new FileList();
    fl.addConfiguredFile(name);

    Path path = new Path(new Project());
    path.addFilelist(fl);

    // Act and Assert
    assertThrows(BuildException.class, () -> JavaCC.getMainClass(path, 1));
  }

  /**
   * Test {@link JavaCC#getMainClass(Path, int)} with {@code path}, {@code type}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code ignore}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaCC#getMainClass(Path, int)}
   */
  @Test
  public void testGetMainClassWithPathType_givenFileNameNameIsIgnore() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("ignore");

    FileList fl = new FileList();
    fl.addConfiguredFile(name);

    Path path = new Path(new Project());
    path.addFilelist(fl);

    // Act and Assert
    assertThrows(BuildException.class, () -> JavaCC.getMainClass(path, 1));
  }

  /**
   * Test {@link JavaCC#getMainClass(Path, int)} with {@code path}, {@code type}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@code null} and path is {@code ignore}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaCC#getMainClass(Path, int)}
   */
  @Test
  public void testGetMainClassWithPathType_whenPathWithPIsNullAndPathIsIgnore() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> JavaCC.getMainClass(new Path(null, "ignore"), 1));
  }

  /**
   * Test {@link JavaCC#getMainClass(Path, int)} with {@code path}, {@code type}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code *}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaCC#getMainClass(Path, int)}
   */
  @Test
  public void testGetMainClassWithPathType_whenPathWithPIsProjectAndPathIsAsterisk() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> JavaCC.getMainClass(new Path(new Project(), "*"), 1));
  }

  /**
   * Test {@link JavaCC#getMainClass(Path, int)} with {@code path}, {@code type}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaCC#getMainClass(Path, int)}
   */
  @Test
  public void testGetMainClassWithPathType_whenPathWithPIsProjectAndPathIsDot() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> JavaCC.getMainClass(new Path(new Project(), "."), 1));
  }

  /**
   * Test {@link JavaCC#getMainClass(Path, int)} with {@code path}, {@code type}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code ignore}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaCC#getMainClass(Path, int)}
   */
  @Test
  public void testGetMainClassWithPathType_whenPathWithPIsProjectAndPathIsIgnore() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> JavaCC.getMainClass(new Path(new Project(), "ignore"), 1));
  }

  /**
   * Test {@link JavaCC#getMainClass(Path, int)} with {@code path}, {@code type}.
   * <ul>
   *   <li>When {@link Path#Path(Project)} with project is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaCC#getMainClass(Path, int)}
   */
  @Test
  public void testGetMainClassWithPathType_whenPathWithProjectIsNull_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> JavaCC.getMainClass(new Path(null), 1));
  }

  /**
   * Test {@link JavaCC#getMainClass(Path, int)} with {@code path}, {@code type}.
   * <ul>
   *   <li>When {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaCC#getMainClass(Path, int)}
   */
  @Test
  public void testGetMainClassWithPathType_whenPathWithProjectIsProject() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> JavaCC.getMainClass(new Path(new Project()), 1));
  }

  /**
   * Test {@link JavaCC#getMajorVersionNumber(File)}.
   * <p>
   * Method under test: {@link JavaCC#getMajorVersionNumber(File)}
   */
  @Test
  public void testGetMajorVersionNumber() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class,
        () -> JavaCC.getMajorVersionNumber(Paths
            .get(System.getProperty("java.io.tmpdir"), "Could not find a path to JavaCC.zip or javacc.jar from '%s'.")
            .toFile()));
  }

  /**
   * Test {@link JavaCC#getMajorVersionNumber(File)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaCC#getMajorVersionNumber(File)}
   */
  @Test
  public void testGetMajorVersionNumber_whenNull() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> JavaCC.getMajorVersionNumber(null));
  }

  /**
   * Test {@link JavaCC#getMajorVersionNumber(File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaCC#getMajorVersionNumber(File)}
   */
  @Test
  public void testGetMajorVersionNumber_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class,
        () -> JavaCC.getMajorVersionNumber(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }
}
