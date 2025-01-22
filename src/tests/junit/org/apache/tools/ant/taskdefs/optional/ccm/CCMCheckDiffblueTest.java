package org.apache.tools.ant.taskdefs.optional.ccm;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintStream;
import java.nio.file.Paths;
import java.util.Vector;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.listener.BigProjectLogger;
import org.apache.tools.ant.types.FileSet;
import org.junit.Test;

public class CCMCheckDiffblueTest {
  /**
   * Test {@link CCMCheck#setFile(File)}.
   * <ul>
   *   <li>Given {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} with three.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCMCheck#setFile(File)}
   */
  @Test
  public void testSetFile_givenByteArrayOutputStreamWithThree() {
    // Arrange
    BigProjectLogger listener = new BigProjectLogger();
    listener.setOutputPrintStream(new PrintStream(new ByteArrayOutputStream(3)));

    Project project = new Project();
    project.addBuildListener(listener);

    CCMCheck ccmCheck = new CCMCheck();
    ccmCheck.setProject(project);
    File v = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    ccmCheck.setFile(v);

    // Assert
    assertSame(v, ccmCheck.getFile());
  }

  /**
   * Test {@link CCMCheck#setFile(File)}.
   * <ul>
   *   <li>Given {@link CCMCheck} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CCMCheck#setFile(File)}
   */
  @Test
  public void testSetFile_givenCCMCheck() {
    // Arrange
    CCMCheck ccmCheck = new CCMCheck();
    File v = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    ccmCheck.setFile(v);

    // Assert
    assertSame(v, ccmCheck.getFile());
  }

  /**
   * Test {@link CCMCheck#setFile(File)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCMCheck#setFile(File)}
   */
  @Test
  public void testSetFile_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Type Name", typeClass);
    project.addBuildListener(new AntClassLoader());

    CCMCheck ccmCheck = new CCMCheck();
    ccmCheck.setProject(project);
    File v = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    ccmCheck.setFile(v);

    // Assert
    assertSame(v, ccmCheck.getFile());
  }

  /**
   * Test {@link CCMCheck#setFile(File)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCMCheck#setFile(File)}
   */
  @Test
  public void testSetFile_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    CCMCheck ccmCheck = new CCMCheck();
    ccmCheck.setProject(project);
    File v = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    ccmCheck.setFile(v);

    // Assert
    assertSame(v, ccmCheck.getFile());
  }

  /**
   * Test {@link CCMCheck#setFile(File)}.
   * <ul>
   *   <li>Then {@link CCMCheck} (default constructor) File is Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCMCheck#setFile(File)}
   */
  @Test
  public void testSetFile_thenCCMCheckFileIsPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange
    CCMCheck ccmCheck = new CCMCheck();
    ccmCheck.setProject(new Project());
    File v = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    ccmCheck.setFile(v);

    // Assert
    assertSame(v, ccmCheck.getFile());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CCMCheck#setComment(String)}
   *   <li>{@link CCMCheck#setTask(String)}
   *   <li>{@link CCMCheck#getComment()}
   *   <li>{@link CCMCheck#getFile()}
   *   <li>{@link CCMCheck#getTask()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    CCMCheck ccmCheck = new CCMCheck();

    // Act
    ccmCheck.setComment("foo");
    ccmCheck.setTask("foo");
    String actualComment = ccmCheck.getComment();
    File actualFile = ccmCheck.getFile();

    // Assert
    assertEquals("foo", actualComment);
    assertEquals("foo", ccmCheck.getTask());
    assertNull(actualFile);
  }

  /**
   * Test {@link CCMCheck#addFileset(FileSet)}.
   * <p>
   * Method under test: {@link CCMCheck#addFileset(FileSet)}
   */
  @Test
  public void testAddFileset() {
    // Arrange
    CCMCheck ccmCheck = new CCMCheck();
    FileSet set = new FileSet();

    // Act
    ccmCheck.addFileset(set);

    // Assert
    Vector<FileSet> fileSetList = ccmCheck.filesets;
    assertEquals(1, fileSetList.size());
    assertSame(set, fileSetList.get(0));
  }

  /**
   * Test {@link CCMCheck#execute()}.
   * <p>
   * Method under test: {@link CCMCheck#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    CCMCheck ccmCheck = new CCMCheck();
    ccmCheck.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    ccmCheck.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> ccmCheck.execute());
  }

  /**
   * Test {@link CCMCheck#execute()}.
   * <p>
   * Method under test: {@link CCMCheck#execute()}
   */
  @Test
  public void testExecute2() throws BuildException {
    // Arrange
    CCMCheck ccmCheck = new CCMCheck();
    ccmCheck.setFile(
        Paths.get(System.getProperty("java.io.tmpdir"), "CCMCheck cannot be generated for directories").toFile());
    ccmCheck.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> ccmCheck.execute());
  }

  /**
   * Test {@link CCMCheck#execute()}.
   * <ul>
   *   <li>Given {@link CCMCheck} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCMCheck#execute()}
   */
  @Test
  public void testExecute_givenCCMCheck_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new CCMCheck()).execute());
  }

  /**
   * Test new {@link CCMCheck} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CCMCheck}
   */
  @Test
  public void testNewCCMCheck() {
    // Arrange and Act
    CCMCheck actualCcmCheck = new CCMCheck();

    // Assert
    assertEquals("", actualCcmCheck.getCcmAction());
    assertEquals("ccm", actualCcmCheck.getCcmCommand());
    assertNull(actualCcmCheck.getFile());
    assertNull(actualCcmCheck.getDescription());
    assertNull(actualCcmCheck.getTaskName());
    assertNull(actualCcmCheck.getTaskType());
    assertNull(actualCcmCheck.getComment());
    assertNull(actualCcmCheck.getTask());
    assertNull(actualCcmCheck.getProject());
    assertNull(actualCcmCheck.getOwningTarget());
    assertTrue(actualCcmCheck.filesets.isEmpty());
  }
}
