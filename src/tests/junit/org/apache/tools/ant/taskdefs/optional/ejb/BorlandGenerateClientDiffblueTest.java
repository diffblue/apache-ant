package org.apache.tools.ant.taskdefs.optional.ejb;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.listener.BigProjectLogger;
import org.apache.tools.ant.taskdefs.Concat;
import org.apache.tools.ant.taskdefs.Tar;
import org.apache.tools.ant.taskdefs.Tar.TarFileSet;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileList.FileName;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.PolyTest;
import org.apache.tools.ant.types.PolyTest.MyPath;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.types.resources.Resources;
import org.apache.tools.ant.types.selectors.ContainsSelector;
import org.junit.Test;

public class BorlandGenerateClientDiffblueTest {
  /**
   * Test {@link BorlandGenerateClient#setClasspath(Path)}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_givenPathWithProjectIsProjectAddFilesetFileSet() throws BuildException {
    // Arrange
    Project project = new Project();

    Path classpath = new Path(project);
    classpath.addFileset(new FileSet());

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(classpath);
    Path classpath2 = Path.systemBootClasspath;
    classpath2.setProject(null);

    // Act
    borlandGenerateClient.setClasspath(classpath2);

    // Assert
    assertSame(project, classpath2.getProject());
  }

  /**
   * Test {@link BorlandGenerateClient#setClasspath(Path)}.
   * <ul>
   *   <li>Then {@link BorlandGenerateClient} (default constructor) {@link BorlandGenerateClient#classpath} Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_thenBorlandGenerateClientClasspathDescriptionIsNull() {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();

    // Act
    borlandGenerateClient.setClasspath(Path.systemBootClasspath);

    // Assert
    Path path = borlandGenerateClient.classpath;
    assertNull(path.getDescription());
    assertNull(path.getRefid());
    assertFalse(path.isReference());
  }

  /**
   * Test {@link BorlandGenerateClient#setClasspath(Path)}.
   * <ul>
   *   <li>Then {@link Path#systemBootClasspath} Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_thenSystemBootClasspathProjectIsNull() {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(new Path(null));
    Path classpath = Path.systemBootClasspath;
    classpath.setProject(null);

    // Act
    borlandGenerateClient.setClasspath(classpath);

    // Assert that nothing has changed
    assertNull(classpath.getProject());
  }

  /**
   * Test {@link BorlandGenerateClient#setClasspath(Path)}.
   * <ul>
   *   <li>Then {@link Path#systemBootClasspath} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_thenSystemBootClasspathProjectIsProject() {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    Project project = new Project();
    borlandGenerateClient.setClasspath(new Path(project));
    Path classpath = Path.systemBootClasspath;
    classpath.setProject(null);

    // Act
    borlandGenerateClient.setClasspath(classpath);

    // Assert
    assertSame(project, classpath.getProject());
  }

  /**
   * Test {@link BorlandGenerateClient#setClasspath(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then not {@link BorlandGenerateClient} (default constructor) {@link BorlandGenerateClient#classpath} Reference.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_whenNull_thenNotBorlandGenerateClientClasspathReference() {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(Path.systemBootClasspath);

    // Act
    borlandGenerateClient.setClasspath(null);

    // Assert that nothing has changed
    assertFalse(borlandGenerateClient.classpath.isReference());
  }

  /**
   * Test {@link BorlandGenerateClient#createClasspath()}.
   * <ul>
   *   <li>Given {@link BorlandGenerateClient} (default constructor).</li>
   *   <li>Then return Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenBorlandGenerateClient_thenReturnProjectIsNull() {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();

    // Act
    Path actualCreateClasspathResult = borlandGenerateClient.createClasspath();

    // Assert
    Path path = borlandGenerateClient.classpath;
    assertNull(path.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(path.getProject());
    assertNull(path.getRefid());
    assertEquals(0, path.size());
    assertFalse(path.isReference());
    assertTrue(path.isEmpty());
  }

  /**
   * Test {@link BorlandGenerateClient#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Then {@link BorlandGenerateClient} (default constructor) {@link BorlandGenerateClient#classpath} Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_thenBorlandGenerateClientClasspathProjectIsNull() {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();

    // Act
    borlandGenerateClient.setClasspathRef(new Reference("42"));

    // Assert
    Path path = borlandGenerateClient.classpath;
    assertNull(path.getDescription());
    assertNull(path.getProject());
    assertNull(path.getRefid());
    assertFalse(path.isReference());
  }

  /**
   * Test {@link BorlandGenerateClient#execute()}.
   * <p>
   * Method under test: {@link BorlandGenerateClient#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    borlandGenerateClient.setClientjar(null);
    borlandGenerateClient.setMode(null);
    borlandGenerateClient.setVersion(5);
    borlandGenerateClient.setDebug(false);

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.execute());
  }

  /**
   * Test {@link BorlandGenerateClient#execute()}.
   * <p>
   * Method under test: {@link BorlandGenerateClient#execute()}
   */
  @Test
  public void testExecute2() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir"), "invalid ejb jar file.").toFile());
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    borlandGenerateClient.setMode(null);
    borlandGenerateClient.setVersion(5);
    borlandGenerateClient.setDebug(false);

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.execute());
  }

  /**
   * Test {@link BorlandGenerateClient#execute()}.
   * <p>
   * Method under test: {@link BorlandGenerateClient#execute()}
   */
  @Test
  public void testExecute3() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir"), "invalid ejb jar file.").toFile());
    borlandGenerateClient
        .setClientjar(Paths.get(System.getProperty("java.io.tmpdir"), "invalid or missing client jar file.").toFile());
    borlandGenerateClient.setMode(null);
    borlandGenerateClient.setVersion(5);
    borlandGenerateClient.setDebug(false);

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.execute());
  }

  /**
   * Test {@link BorlandGenerateClient#execute()}.
   * <p>
   * Method under test: {@link BorlandGenerateClient#execute()}
   */
  @Test
  public void testExecute4() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient
        .setEjbjar(Paths.get(System.getProperty("java.io.tmpdir"), "invalid ejb jar file.", "foo").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.execute());
  }

  /**
   * Test {@link BorlandGenerateClient#execute()}.
   * <ul>
   *   <li>Given {@link BorlandGenerateClient} (default constructor) Classpath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#execute()}
   */
  @Test
  public void testExecute_givenBorlandGenerateClientClasspathIsSystemBootClasspath() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(Path.systemBootClasspath);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir"), "invalid ejb jar file.").toFile());
    borlandGenerateClient.setClientjar(null);
    borlandGenerateClient.setMode("fork");
    borlandGenerateClient.setVersion(5);
    borlandGenerateClient.setDebug(false);

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.execute());
  }

  /**
   * Test {@link BorlandGenerateClient#execute()}.
   * <ul>
   *   <li>Given {@link BorlandGenerateClient} (default constructor) Clientjar is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#execute()}
   */
  @Test
  public void testExecute_givenBorlandGenerateClientClientjarIsNull_thenThrowBuildException() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir"), "invalid ejb jar file.").toFile());
    borlandGenerateClient.setClientjar(null);
    borlandGenerateClient.setMode(null);
    borlandGenerateClient.setVersion(5);
    borlandGenerateClient.setDebug(false);

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.execute());
  }

  /**
   * Test {@link BorlandGenerateClient#execute()}.
   * <ul>
   *   <li>Given {@link BorlandGenerateClient} (default constructor) Debug is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#execute()}
   */
  @Test
  public void testExecute_givenBorlandGenerateClientDebugIsTrue_thenThrowBuildException() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient
        .setEjbjar(Paths.get(System.getProperty("java.io.tmpdir"), "invalid ejb jar file.", "foo").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.execute());
  }

  /**
   * Test {@link BorlandGenerateClient#execute()}.
   * <ul>
   *   <li>Given {@link BorlandGenerateClient} (default constructor) Mode is {@code invalid or missing client jar file.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#execute()}
   */
  @Test
  public void testExecute_givenBorlandGenerateClientModeIsInvalidOrMissingClientJarFile() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir"), "invalid ejb jar file.").toFile());
    borlandGenerateClient.setClientjar(null);
    borlandGenerateClient.setMode("invalid or missing client jar file.");
    borlandGenerateClient.setVersion(5);
    borlandGenerateClient.setDebug(false);

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.execute());
  }

  /**
   * Test {@link BorlandGenerateClient#execute()}.
   * <ul>
   *   <li>Given {@link BorlandGenerateClient} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#execute()}
   */
  @Test
  public void testExecute_givenBorlandGenerateClientProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setProject(new Project());
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir"), "invalid ejb jar file.").toFile());
    borlandGenerateClient.setClientjar(null);
    borlandGenerateClient.setMode(null);
    borlandGenerateClient.setVersion(5);
    borlandGenerateClient.setDebug(false);

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.execute());
  }

  /**
   * Test {@link BorlandGenerateClient#execute()}.
   * <ul>
   *   <li>Given {@link BorlandGenerateClient} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#execute()}
   */
  @Test
  public void testExecute_givenBorlandGenerateClientProjectIsProject_thenThrowBuildException2() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setProject(new Project());
    borlandGenerateClient
        .setEjbjar(Paths.get(System.getProperty("java.io.tmpdir"), "invalid ejb jar file.", "foo").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.execute());
  }

  /**
   * Test {@link BorlandGenerateClient#execute()}.
   * <ul>
   *   <li>Given {@link BorlandGenerateClient} (default constructor) Version is three.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#execute()}
   */
  @Test
  public void testExecute_givenBorlandGenerateClientVersionIsThree_thenThrowBuildException() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir"), "invalid ejb jar file.").toFile());
    borlandGenerateClient.setClientjar(null);
    borlandGenerateClient.setMode(null);
    borlandGenerateClient.setVersion(3);
    borlandGenerateClient.setDebug(false);

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.execute());
  }

  /**
   * Test {@link BorlandGenerateClient#execute()}.
   * <ul>
   *   <li>Given {@link BorlandGenerateClient} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#execute()}
   */
  @Test
  public void testExecute_givenBorlandGenerateClient_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BorlandGenerateClient()).execute());
  }

  /**
   * Test {@link BorlandGenerateClient#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setProject(project);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir"), "invalid ejb jar file.").toFile());
    borlandGenerateClient.setClientjar(null);
    borlandGenerateClient.setMode(null);
    borlandGenerateClient.setVersion(5);
    borlandGenerateClient.setDebug(false);

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.execute());
  }

  /**
   * Test {@link BorlandGenerateClient#executeJava()}.
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeJava()}
   */
  @Test
  public void testExecuteJava() throws BuildException {
    // Arrange
    FileSet fs = new FileSet();
    fs.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "mode : java", "java").toFile());

    Path classpath = new Path(null);
    classpath.addFileset(fs);

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setClasspath(classpath);

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeJava());
  }

  /**
   * Test {@link BorlandGenerateClient#executeJava()}.
   * <ul>
   *   <li>Given {@link BorlandGenerateClient} (default constructor) Version is five.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeJava()}
   */
  @Test
  public void testExecuteJava_givenBorlandGenerateClientVersionIsFive_thenThrowBuildException() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setVersion(5);
    borlandGenerateClient.setDebug(false);

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeJava());
  }

  /**
   * Test {@link BorlandGenerateClient#executeJava()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code mode : java}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeJava()}
   */
  @Test
  public void testExecuteJava_givenFileNameNameIsModeJava_thenThrowBuildException() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("mode : java");

    FileList fl = new FileList();
    fl.addConfiguredFile(name);

    Path classpath = new Path(null);
    classpath.addFilelist(fl);
    classpath.addFileset(new FileSet());

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setClasspath(classpath);

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeJava());
  }

  /**
   * Test {@link BorlandGenerateClient#executeJava()}.
   * <ul>
   *   <li>Given {@link FileSet#FileSet()} addContains {@link ContainsSelector} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeJava()}
   */
  @Test
  public void testExecuteJava_givenFileSetAddContainsContainsSelector_thenThrowBuildException() throws BuildException {
    // Arrange
    FileSet fs = new FileSet();
    fs.addContains(new ContainsSelector());
    fs.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    Path classpath = new Path(null);
    classpath.setProject(new Project());
    classpath.addFileset(fs);

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setClasspath(classpath);

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeJava());
  }

  /**
   * Test {@link BorlandGenerateClient#executeJava()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code mode : java} addFileset {@link FileSet#FileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeJava()}
   */
  @Test
  public void testExecuteJava_givenPathWithPIsProjectAndPathIsModeJavaAddFilesetFileSet() throws BuildException {
    // Arrange
    Path classpath = new Path(new Project(), "mode : java");
    classpath.addFileset(new FileSet());

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setClasspath(classpath);

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeJava());
  }

  /**
   * Test {@link BorlandGenerateClient#executeJava()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@code null} addFilelist {@link FileList#FileList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeJava()}
   */
  @Test
  public void testExecuteJava_givenPathWithProjectIsNullAddFilelistFileList() throws BuildException {
    // Arrange
    Path classpath = new Path(null);
    classpath.addFilelist(new FileList());
    classpath.addFileset(new FileSet());

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setClasspath(classpath);

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeJava());
  }

  /**
   * Test {@link BorlandGenerateClient#executeJava()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@code null} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeJava()}
   */
  @Test
  public void testExecuteJava_givenPathWithProjectIsNullProjectIsProject() throws BuildException {
    // Arrange
    Path classpath = new Path(null);
    classpath.setProject(new Project());
    classpath.addFileset(new FileSet());

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setClasspath(classpath);

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeJava());
  }

  /**
   * Test {@link BorlandGenerateClient#executeJava()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeJava()}
   */
  @Test
  public void testExecuteJava_thenThrowBuildException() throws BuildException {
    // Arrange
    Path classpath = new Path(null);
    classpath.addFileset(new FileSet());

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setClasspath(classpath);

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeJava());
  }

  /**
   * Test {@link BorlandGenerateClient#executeFork()}.
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeFork()}
   */
  @Test
  public void testExecuteFork() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(new Path(new Project(), "mode : fork 5"));
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setVersion(5);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeFork());
  }

  /**
   * Test {@link BorlandGenerateClient#executeFork()}.
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeFork()}
   */
  @Test
  public void testExecuteFork2() throws BuildException {
    // Arrange
    Path classpath = new Path(new Project());
    classpath.add(new Path(new Project(), "mode : fork 5"));

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(classpath);
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setVersion(5);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeFork());
  }

  /**
   * Test {@link BorlandGenerateClient#executeFork()}.
   * <ul>
   *   <li>Given {@link BorlandGenerateClient} (default constructor) Classpath is {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeFork()}
   */
  @Test
  public void testExecuteFork_givenBorlandGenerateClientClasspathIsPathWithProjectIsProject() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(new Path(new Project()));
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setVersion(5);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeFork());
  }

  /**
   * Test {@link BorlandGenerateClient#executeFork()}.
   * <ul>
   *   <li>Given {@link BorlandGenerateClient} (default constructor) Classpath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeFork()}
   */
  @Test
  public void testExecuteFork_givenBorlandGenerateClientClasspathIsSystemBootClasspath() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(Path.systemBootClasspath);
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setVersion(5);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeFork());
  }

  /**
   * Test {@link BorlandGenerateClient#executeFork()}.
   * <ul>
   *   <li>Given {@link BorlandGenerateClient} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeFork()}
   */
  @Test
  public void testExecuteFork_givenBorlandGenerateClientProjectIsProject() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setProject(new Project());
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setVersion(4);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeFork());
  }

  /**
   * Test {@link BorlandGenerateClient#executeFork()}.
   * <ul>
   *   <li>Given {@link BorlandGenerateClient} (default constructor) Version is four.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeFork()}
   */
  @Test
  public void testExecuteFork_givenBorlandGenerateClientVersionIsFour_thenThrowBuildException() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setVersion(4);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeFork());
  }

  /**
   * Test {@link BorlandGenerateClient#executeFork()}.
   * <ul>
   *   <li>Given {@link BorlandGenerateClient} (default constructor) Version is four.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeFork()}
   */
  @Test
  public void testExecuteFork_givenBorlandGenerateClientVersionIsFour_thenThrowBuildException2() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setVersion(4);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeFork());
  }

  /**
   * Test {@link BorlandGenerateClient#executeFork()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code mode : fork 5}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeFork()}
   */
  @Test
  public void testExecuteFork_givenFileNameNameIsModeFork5_thenThrowBuildException() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("mode : fork 5");

    FileList fl = new FileList();
    fl.addConfiguredFile(name);

    Path classpath = new Path(new Project());
    classpath.addFilelist(fl);

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(classpath);
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setVersion(5);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeFork());
  }

  /**
   * Test {@link BorlandGenerateClient#executeFork()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeFork()}
   */
  @Test
  public void testExecuteFork_givenJavaLangObject_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("redirector", typeClass);
    project.addBuildListener(new AntClassLoader());

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setProject(project);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setVersion(4);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeFork());
  }

  /**
   * Test {@link BorlandGenerateClient#executeFork()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) add {@link Concat} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeFork()}
   */
  @Test
  public void testExecuteFork_givenPathWithProjectIsProjectAddConcat_thenThrowBuildException() throws BuildException {
    // Arrange
    Path classpath = new Path(new Project());
    classpath.add(new Concat());

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(classpath);
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setVersion(5);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeFork());
  }

  /**
   * Test {@link BorlandGenerateClient#executeFork()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeFork()}
   */
  @Test
  public void testExecuteFork_givenPathWithProjectIsProjectAddFilelistFileList() throws BuildException {
    // Arrange
    Path classpath = new Path(new Project());
    classpath.addFilelist(new FileList());

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(classpath);
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setVersion(5);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeFork());
  }

  /**
   * Test {@link BorlandGenerateClient#executeFork()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) add {@link MyPath#MyPath(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeFork()}
   */
  @Test
  public void testExecuteFork_givenPathWithProjectIsProjectAddMyPathWithProjectIsProject() throws BuildException {
    // Arrange
    Path classpath = new Path(new Project());
    classpath.add(new MyPath(new Project()));

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(classpath);
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setVersion(5);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeFork());
  }

  /**
   * Test {@link BorlandGenerateClient#executeFork()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeFork()}
   */
  @Test
  public void testExecuteFork_givenPathWithProjectIsProjectAddNone_thenThrowBuildException() throws BuildException {
    // Arrange
    Path classpath = new Path(new Project());
    classpath.add(Resources.NONE);

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(classpath);
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setVersion(5);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeFork());
  }

  /**
   * Test {@link BorlandGenerateClient#executeFork()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) add {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeFork()}
   */
  @Test
  public void testExecuteFork_givenPathWithProjectIsProjectAddPathWithProjectIsProject() throws BuildException {
    // Arrange
    Path classpath = new Path(new Project());
    classpath.add(new Path(new Project()));

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(classpath);
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setVersion(5);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeFork());
  }

  /**
   * Test {@link BorlandGenerateClient#executeFork()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) add {@link Tar.TarFileSet#TarFileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeFork()}
   */
  @Test
  public void testExecuteFork_givenPathWithProjectIsProjectAddTarFileSet() throws BuildException {
    // Arrange
    Path classpath = new Path(new Project());
    classpath.add(new TarFileSet());

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(classpath);
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setVersion(5);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeFork());
  }

  /**
   * Test {@link BorlandGenerateClient#executeFork()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeFork()}
   */
  @Test
  public void testExecuteFork_givenProjectAddBuildListenerAntClassLoader() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setProject(project);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setVersion(4);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeFork());
  }

  /**
   * Test {@link BorlandGenerateClient#executeForkV4()}.
   * <ul>
   *   <li>Given {@link BigProjectLogger} (default constructor) MessageOutputLevel is four.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeForkV4()}
   */
  @Test
  public void testExecuteForkV4_givenBigProjectLoggerMessageOutputLevelIsFour() throws BuildException {
    // Arrange
    BigProjectLogger listener = new BigProjectLogger();
    listener.setMessageOutputLevel(4);
    listener.setOutputPrintStream(new PrintStream(new ByteArrayOutputStream(4)));

    Project project = new Project();
    project.addBuildListener(listener);

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    borlandGenerateClient.setProject(project);
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeForkV4());
  }

  /**
   * Test {@link BorlandGenerateClient#executeForkV4()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeForkV4()}
   */
  @Test
  public void testExecuteForkV4_givenJavaLangObject_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("redirector", typeClass);
    project.addBuildListener(new AntClassLoader());

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    borlandGenerateClient.setProject(project);
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeForkV4());
  }

  /**
   * Test {@link BorlandGenerateClient#executeForkV4()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeForkV4()}
   */
  @Test
  public void testExecuteForkV4_thenThrowBuildException() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeForkV4());
  }

  /**
   * Test {@link BorlandGenerateClient#executeForkV4()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeForkV4()}
   */
  @Test
  public void testExecuteForkV4_thenThrowBuildException2() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    borlandGenerateClient.setProject(project);
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeForkV4());
  }

  /**
   * Test {@link BorlandGenerateClient#executeForkV5()}.
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeForkV5()}
   */
  @Test
  public void testExecuteForkV5() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(new Path(new Project(), "mode : fork 5"));
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeForkV5());
  }

  /**
   * Test {@link BorlandGenerateClient#executeForkV5()}.
   * <ul>
   *   <li>Given {@link BorlandGenerateClient} (default constructor) Classpath is {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeForkV5()}
   */
  @Test
  public void testExecuteForkV5_givenBorlandGenerateClientClasspathIsPathWithProjectIsProject() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(new Path(new Project()));
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeForkV5());
  }

  /**
   * Test {@link BorlandGenerateClient#executeForkV5()}.
   * <ul>
   *   <li>Given {@link BorlandGenerateClient} (default constructor) Classpath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeForkV5()}
   */
  @Test
  public void testExecuteForkV5_givenBorlandGenerateClientClasspathIsSystemBootClasspath() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(Path.systemBootClasspath);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeForkV5());
  }

  /**
   * Test {@link BorlandGenerateClient#executeForkV5()}.
   * <ul>
   *   <li>Given {@link BorlandGenerateClient} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeForkV5()}
   */
  @Test
  public void testExecuteForkV5_givenBorlandGenerateClientProjectIsProject() throws BuildException {
    // Arrange
    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setProject(new Project());
    borlandGenerateClient.setClasspath(new Path(new Project()));
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeForkV5());
  }

  /**
   * Test {@link BorlandGenerateClient#executeForkV5()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code mode : fork 5}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeForkV5()}
   */
  @Test
  public void testExecuteForkV5_givenFileNameNameIsModeFork5_thenThrowBuildException() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("mode : fork 5");

    FileList fl = new FileList();
    fl.addConfiguredFile(name);

    Path classpath = new Path(new Project());
    classpath.addFilelist(fl);
    classpath.addFileset(new FileSet());

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(classpath);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeForkV5());
  }

  /**
   * Test {@link BorlandGenerateClient#executeForkV5()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@code null} addFileset {@link FileSet#FileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeForkV5()}
   */
  @Test
  public void testExecuteForkV5_givenPathWithProjectIsNullAddFilesetFileSet() throws BuildException {
    // Arrange
    Path classpath = new Path(null);
    classpath.addFileset(new FileSet());

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(classpath);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeForkV5());
  }

  /**
   * Test {@link BorlandGenerateClient#executeForkV5()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeForkV5()}
   */
  @Test
  public void testExecuteForkV5_givenPathWithProjectIsProjectAddFilelistFileList() throws BuildException {
    // Arrange
    Path classpath = new Path(new Project());
    classpath.addFilelist(new FileList());
    classpath.addFileset(new FileSet());

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(classpath);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeForkV5());
  }

  /**
   * Test {@link BorlandGenerateClient#executeForkV5()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeForkV5()}
   */
  @Test
  public void testExecuteForkV5_givenPathWithProjectIsProjectAddFilesetFileSet() throws BuildException {
    // Arrange
    Path classpath = new Path(new Project());
    classpath.addFileset(new FileSet());

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(classpath);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeForkV5());
  }

  /**
   * Test {@link BorlandGenerateClient#executeForkV5()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) add {@link MyPath#MyPath(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeForkV5()}
   */
  @Test
  public void testExecuteForkV5_givenPathWithProjectIsProjectAddMyPathWithProjectIsProject() throws BuildException {
    // Arrange
    Path classpath = new Path(new Project());
    classpath.add(new MyPath(new Project()));
    classpath.addFileset(new FileSet());

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(classpath);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeForkV5());
  }

  /**
   * Test {@link BorlandGenerateClient#executeForkV5()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) add {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeForkV5()}
   */
  @Test
  public void testExecuteForkV5_givenPathWithProjectIsProjectAddPathWithProjectIsProject() throws BuildException {
    // Arrange
    Path classpath = new Path(new Project());
    classpath.add(new Path(new Project()));
    classpath.addFileset(new FileSet());

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(classpath);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeForkV5());
  }

  /**
   * Test {@link BorlandGenerateClient#executeForkV5()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeForkV5()}
   */
  @Test
  public void testExecuteForkV5_givenProjectAddBuildListenerAntClassLoader() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Path classpath = new Path(project);
    classpath.addFileset(new FileSet());

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(classpath);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeForkV5());
  }

  /**
   * Test {@link BorlandGenerateClient#executeForkV5()}.
   * <p>
   * Method under test: {@link BorlandGenerateClient#executeForkV5()}
   */
  @Test
  public void testExecuteForkV52() throws BuildException {
    // Arrange
    Path classpath = new Path(new Project());
    classpath.add(new Path(new Project(), "mode : fork 5"));
    classpath.addFileset(new FileSet());

    BorlandGenerateClient borlandGenerateClient = new BorlandGenerateClient();
    borlandGenerateClient.setClasspath(classpath);
    borlandGenerateClient.setClientjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());
    borlandGenerateClient.setDebug(true);
    borlandGenerateClient.setEjbjar(Paths.get(System.getProperty("java.io.tmpdir")).toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandGenerateClient.executeForkV5());
  }

  /**
   * Test new {@link BorlandGenerateClient} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link BorlandGenerateClient}
   */
  @Test
  public void testNewBorlandGenerateClient() {
    // Arrange and Act
    BorlandGenerateClient actualBorlandGenerateClient = new BorlandGenerateClient();

    // Assert
    assertEquals("fork", actualBorlandGenerateClient.mode);
    assertNull(actualBorlandGenerateClient.clientjarfile);
    assertNull(actualBorlandGenerateClient.ejbjarfile);
    assertNull(actualBorlandGenerateClient.getDescription());
    assertNull(actualBorlandGenerateClient.getTaskName());
    assertNull(actualBorlandGenerateClient.getTaskType());
    assertNull(actualBorlandGenerateClient.getProject());
    assertNull(actualBorlandGenerateClient.getOwningTarget());
    assertNull(actualBorlandGenerateClient.classpath);
    assertEquals(4, actualBorlandGenerateClient.version);
    assertFalse(actualBorlandGenerateClient.debug);
  }
}
