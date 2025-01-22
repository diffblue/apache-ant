package org.apache.tools.ant.taskdefs.optional;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import java.util.Vector;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.taskdefs.ExecTask;
import org.apache.tools.ant.types.FileSet;
import org.junit.Test;

public class CabDiffblueTest {
  /**
   * Test {@link Cab#addFileset(FileSet)}.
   * <ul>
   *   <li>Given {@link Cab} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Cab#addFileset(FileSet)}
   */
  @Test
  public void testAddFileset_givenCabAddFilesetFileSet_thenThrowBuildException() {
    // Arrange
    Cab cab = new Cab();
    cab.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> cab.addFileset(new FileSet()));
  }

  /**
   * Test {@link Cab#checkConfiguration()}.
   * <p>
   * Method under test: {@link Cab#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration() throws BuildException {
    // Arrange
    Cab cab = new Cab();
    cab.setBasedir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    cab.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> cab.checkConfiguration());
  }

  /**
   * Test {@link Cab#checkConfiguration()}.
   * <p>
   * Method under test: {@link Cab#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration2() throws BuildException {
    // Arrange
    Cab cab = new Cab();
    cab.setBasedir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> cab.checkConfiguration());
  }

  /**
   * Test {@link Cab#checkConfiguration()}.
   * <p>
   * Method under test: {@link Cab#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration3() throws BuildException {
    // Arrange
    Cab cab = new Cab();
    cab.setBasedir(
        Paths.get(System.getProperty("java.io.tmpdir"), "Both basedir attribute and a nested fileset is not allowed")
            .toFile());
    cab.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> cab.checkConfiguration());
  }

  /**
   * Test {@link Cab#checkConfiguration()}.
   * <ul>
   *   <li>Given {@link Cab} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Cab#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenCabAddFilesetFileSet_thenThrowBuildException() throws BuildException {
    // Arrange
    Cab cab = new Cab();
    cab.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> cab.checkConfiguration());
  }

  /**
   * Test {@link Cab#checkConfiguration()}.
   * <ul>
   *   <li>Given {@link Cab} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Cab#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenCab_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Cab()).checkConfiguration());
  }

  /**
   * Test {@link Cab#createExec()}.
   * <ul>
   *   <li>Given {@link Cab} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Cab#createExec()}
   */
  @Test
  public void testCreateExec_givenCab() throws BuildException {
    // Arrange and Act
    ExecTask actualCreateExecResult = (new Cab()).createExec();

    // Assert
    assertNull(actualCreateExecResult.getDescription());
    assertNull(actualCreateExecResult.getTaskName());
    assertNull(actualCreateExecResult.getTaskType());
    assertNull(actualCreateExecResult.getOs());
    assertNull(actualCreateExecResult.getOsFamily());
    assertNull(actualCreateExecResult.getProject());
    assertNull(actualCreateExecResult.getOwningTarget());
    assertFalse(actualCreateExecResult.getResolveExecutable());
  }

  /**
   * Test {@link Cab#createExec()}.
   * <ul>
   *   <li>Given {@link Cab} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Cab#createExec()}
   */
  @Test
  public void testCreateExec_givenCabAddFilesetFileSet() throws BuildException {
    // Arrange
    Cab cab = new Cab();
    cab.addFileset(new FileSet());

    // Act
    ExecTask actualCreateExecResult = cab.createExec();

    // Assert
    assertNull(actualCreateExecResult.getDescription());
    assertNull(actualCreateExecResult.getTaskName());
    assertNull(actualCreateExecResult.getTaskType());
    assertNull(actualCreateExecResult.getOs());
    assertNull(actualCreateExecResult.getOsFamily());
    assertNull(actualCreateExecResult.getProject());
    assertNull(actualCreateExecResult.getOwningTarget());
    assertFalse(actualCreateExecResult.getResolveExecutable());
  }

  /**
   * Test {@link Cab#isUpToDate(Vector)}.
   * <ul>
   *   <li>Given {@code ..}.</li>
   *   <li>When {@link Vector#Vector()} add {@code ..}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Cab#isUpToDate(Vector)}
   */
  @Test
  public void testIsUpToDate_givenDotDot_whenVectorAddDotDot_thenReturnTrue() {
    // Arrange
    Cab cab = new Cab();
    cab.setCabfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    Vector<String> files = new Vector<>();
    files.add("..");

    // Act and Assert
    assertTrue(cab.isUpToDate(files));
  }

  /**
   * Test {@link Cab#isUpToDate(Vector)}.
   * <ul>
   *   <li>Given {@code .}.</li>
   *   <li>When {@link Vector#Vector()} add {@code .}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Cab#isUpToDate(Vector)}
   */
  @Test
  public void testIsUpToDate_givenDot_whenVectorAddDot_thenReturnTrue() {
    // Arrange
    Cab cab = new Cab();
    cab.setCabfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    Vector<String> files = new Vector<>();
    files.add(".");

    // Act and Assert
    assertTrue(cab.isUpToDate(files));
  }

  /**
   * Test {@link Cab#isUpToDate(Vector)}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>When {@link Vector#Vector()} add empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Cab#isUpToDate(Vector)}
   */
  @Test
  public void testIsUpToDate_givenEmptyString_whenVectorAddEmptyString_thenReturnTrue() {
    // Arrange
    Cab cab = new Cab();
    cab.setCabfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    Vector<String> files = new Vector<>();
    files.add("");

    // Act and Assert
    assertTrue(cab.isUpToDate(files));
  }

  /**
   * Test {@link Cab#isUpToDate(Vector)}.
   * <ul>
   *   <li>Given {@code foo}.</li>
   *   <li>When {@link Vector#Vector()} add {@code foo}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Cab#isUpToDate(Vector)}
   */
  @Test
  public void testIsUpToDate_givenFoo_whenVectorAddFoo_thenReturnTrue() {
    // Arrange
    Cab cab = new Cab();
    cab.setCabfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    Vector<String> files = new Vector<>();
    files.add("foo");

    // Act and Assert
    assertTrue(cab.isUpToDate(files));
  }

  /**
   * Test {@link Cab#isUpToDate(Vector)}.
   * <ul>
   *   <li>Given {@code Users}.</li>
   *   <li>When {@link Vector#Vector()} add {@code Users}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Cab#isUpToDate(Vector)}
   */
  @Test
  public void testIsUpToDate_givenUsers_whenVectorAddUsers_thenReturnTrue() {
    // Arrange
    Cab cab = new Cab();
    cab.setCabfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    Vector<String> files = new Vector<>();
    files.add("Users");
    files.add("foo");

    // Act and Assert
    assertTrue(cab.isUpToDate(files));
  }

  /**
   * Test {@link Cab#isUpToDate(Vector)}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Cab#isUpToDate(Vector)}
   */
  @Test
  public void testIsUpToDate_thenReturnFalse() {
    // Arrange
    Cab cab = new Cab();
    cab.setCabfile(Paths.get(System.getProperty("java.io.tmpdir"), "Users").toFile());

    Vector<String> files = new Vector<>();
    files.add("foo");

    // Act and Assert
    assertFalse(cab.isUpToDate(files));
  }

  /**
   * Test {@link Cab#isUpToDate(Vector)}.
   * <ul>
   *   <li>When {@link Vector#Vector()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Cab#isUpToDate(Vector)}
   */
  @Test
  public void testIsUpToDate_whenVector_thenReturnTrue() {
    // Arrange
    Cab cab = new Cab();
    cab.setCabfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertTrue(cab.isUpToDate(new Vector<>()));
  }

  /**
   * Test {@link Cab#getFileList()}.
   * <ul>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Cab#getFileList()}
   */
  @Test
  public void testGetFileList_thenReturnEmpty() throws BuildException {
    // Arrange
    Cab cab = new Cab();
    cab.setProject(new Project());
    cab.setBasedir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertTrue(cab.getFileList().isEmpty());
  }

  /**
   * Test {@link Cab#execute()}.
   * <p>
   * Method under test: {@link Cab#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    Cab cab = new Cab();
    cab.setBasedir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    cab.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> cab.execute());
  }

  /**
   * Test {@link Cab#execute()}.
   * <p>
   * Method under test: {@link Cab#execute()}
   */
  @Test
  public void testExecute2() throws BuildException {
    // Arrange
    Cab cab = new Cab();
    cab.setBasedir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> cab.execute());
  }

  /**
   * Test {@link Cab#execute()}.
   * <p>
   * Method under test: {@link Cab#execute()}
   */
  @Test
  public void testExecute3() throws BuildException {
    // Arrange
    Cab cab = new Cab();
    cab.setBasedir(
        Paths.get(System.getProperty("java.io.tmpdir"), "Both basedir attribute and a nested fileset is not allowed")
            .toFile());
    cab.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> cab.execute());
  }

  /**
   * Test {@link Cab#execute()}.
   * <ul>
   *   <li>Given {@link Cab} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Cab#execute()}
   */
  @Test
  public void testExecute_givenCabAddFilesetFileSet_thenThrowBuildException() throws BuildException {
    // Arrange
    Cab cab = new Cab();
    cab.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> cab.execute());
  }

  /**
   * Test {@link Cab#execute()}.
   * <ul>
   *   <li>Given {@link Cab} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Cab#execute()}
   */
  @Test
  public void testExecute_givenCab_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Cab()).execute());
  }

  /**
   * Test new {@link Cab} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Cab}
   */
  @Test
  public void testNewCab() {
    // Arrange and Act
    Cab actualCab = new Cab();

    // Assert
    assertEquals("cab", actualCab.archiveType);
    Location location = actualCab.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCab.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualCab.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualCab.getTaskName());
    assertNull(actualCab.getTaskType());
    assertNull(actualCab.getProject());
    assertNull(actualCab.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualCab.hasSelectors());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualCab, runtimeConfigurableWrapper.getProxy());
  }
}
