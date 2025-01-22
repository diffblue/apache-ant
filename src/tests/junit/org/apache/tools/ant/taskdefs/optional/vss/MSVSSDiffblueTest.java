package org.apache.tools.ant.taskdefs.optional.vss;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.optional.vss.MSVSS.CurrentModUpdated;
import org.apache.tools.ant.taskdefs.optional.vss.MSVSS.WritableFiles;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class MSVSSDiffblueTest {
  /**
   * Test CurrentModUpdated {@link CurrentModUpdated#getValues()}.
   * <p>
   * Method under test: {@link CurrentModUpdated#getValues()}
   */
  @Test
  public void testCurrentModUpdatedGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(
        new String[]{MSVSSConstants.TIME_CURRENT, MSVSSConstants.TIME_MODIFIED, MSVSSConstants.TIME_UPDATED},
        (new CurrentModUpdated()).getValues());
  }

  /**
   * Test CurrentModUpdated new {@link CurrentModUpdated} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CurrentModUpdated}
   */
  @Test
  public void testCurrentModUpdatedNewCurrentModUpdated() {
    // Arrange and Act
    CurrentModUpdated actualCurrentModUpdated = new CurrentModUpdated();

    // Assert
    assertNull(actualCurrentModUpdated.getValue());
    assertEquals(-1, actualCurrentModUpdated.getIndex());
  }

  /**
   * Test {@link MSVSS#execute()}.
   * <p>
   * Method under test: {@link MSVSS#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    Project project = new Project();
    project.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    project.addBuildListener(new AntClassLoader());

    MSVSSADD msvssadd = new MSVSSADD();
    msvssadd.setProject(project);
    msvssadd.setLocalpath(new Path(new Project()));

    // Act and Assert
    assertThrows(BuildException.class, () -> msvssadd.execute());
  }

  /**
   * Test {@link MSVSS#execute()}.
   * <ul>
   *   <li>Given {@link MSVSSADD} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSS#execute()}
   */
  @Test
  public void testExecute_givenMsvssaddProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    MSVSSADD msvssadd = new MSVSSADD();
    msvssadd.setProject(new Project());
    msvssadd.setLocalpath(new Path(new Project()));

    // Act and Assert
    assertThrows(BuildException.class, () -> msvssadd.execute());
  }

  /**
   * Test {@link MSVSS#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSS#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    MSVSSADD msvssadd = new MSVSSADD();
    msvssadd.setProject(project);
    msvssadd.setLocalpath(new Path(new Project()));

    // Act and Assert
    assertThrows(BuildException.class, () -> msvssadd.execute());
  }

  /**
   * Test {@link MSVSS#getLabel()}.
   * <ul>
   *   <li>Given {@link MSVSSADD} (default constructor) InternalLabel is empty string.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSS#getLabel()}
   */
  @Test
  public void testGetLabel_givenMsvssaddInternalLabelIsEmptyString_thenReturnEmptyString() {
    // Arrange
    MSVSSADD msvssadd = new MSVSSADD();
    msvssadd.setInternalLabel("");

    // Act and Assert
    assertEquals("", msvssadd.getLabel());
  }

  /**
   * Test {@link MSVSS#getLabel()}.
   * <ul>
   *   <li>Given {@link MSVSSADD} (default constructor) InternalLabel is {@code foo}.</li>
   *   <li>Then return {@code -Lfoo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSS#getLabel()}
   */
  @Test
  public void testGetLabel_givenMsvssaddInternalLabelIsFoo_thenReturnLfoo() {
    // Arrange
    MSVSSADD msvssadd = new MSVSSADD();
    msvssadd.setInternalLabel("foo");

    // Act and Assert
    assertEquals("-Lfoo", msvssadd.getLabel());
  }

  /**
   * Test {@link MSVSS#getLabel()}.
   * <ul>
   *   <li>Given {@link MSVSSADD} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSS#getLabel()}
   */
  @Test
  public void testGetLabel_givenMsvssadd_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new MSVSSADD()).getLabel());
  }

  /**
   * Test {@link MSVSS#getComment()}.
   * <ul>
   *   <li>Given {@link MSVSSADD} (default constructor) InternalComment is {@code foo}.</li>
   *   <li>Then return {@code -Cfoo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSS#getComment()}
   */
  @Test
  public void testGetComment_givenMsvssaddInternalCommentIsFoo_thenReturnCfoo() {
    // Arrange
    MSVSSADD msvssadd = new MSVSSADD();
    msvssadd.setInternalComment("foo");

    // Act and Assert
    assertEquals("-Cfoo", msvssadd.getComment());
  }

  /**
   * Test {@link MSVSS#getComment()}.
   * <ul>
   *   <li>Given {@link MSVSSADD} (default constructor).</li>
   *   <li>Then return {@code -C-}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSS#getComment()}
   */
  @Test
  public void testGetComment_givenMsvssadd_thenReturnC() {
    // Arrange, Act and Assert
    assertEquals("-C-", (new MSVSSADD()).getComment());
  }

  /**
   * Test {@link MSVSS#getAutoresponse()}.
   * <p>
   * Method under test: {@link MSVSS#getAutoresponse()}
   */
  @Test
  public void testGetAutoresponse() {
    // Arrange, Act and Assert
    assertEquals(MSVSSConstants.FLAG_AUTORESPONSE_DEF, (new MSVSSADD()).getAutoresponse());
  }

  /**
   * Test {@link MSVSS#getLogin()}.
   * <ul>
   *   <li>Given {@link MSVSSADD} (default constructor) Login is {@code foo}.</li>
   *   <li>Then return {@code -Yfoo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSS#getLogin()}
   */
  @Test
  public void testGetLogin_givenMsvssaddLoginIsFoo_thenReturnYfoo() {
    // Arrange
    MSVSSADD msvssadd = new MSVSSADD();
    msvssadd.setLogin("foo");

    // Act and Assert
    assertEquals("-Yfoo", msvssadd.getLogin());
  }

  /**
   * Test {@link MSVSS#getLogin()}.
   * <ul>
   *   <li>Given {@link MSVSSADD} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSS#getLogin()}
   */
  @Test
  public void testGetLogin_givenMsvssadd_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new MSVSSADD()).getLogin());
  }

  /**
   * Test {@link MSVSS#getGetLocalCopy()}.
   * <ul>
   *   <li>Given {@link MSVSSADD} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSS#getGetLocalCopy()}
   */
  @Test
  public void testGetGetLocalCopy_givenMsvssadd_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new MSVSSADD()).getGetLocalCopy());
  }

  /**
   * Test {@link MSVSS#getGetLocalCopy()}.
   * <ul>
   *   <li>Then return {@link MSVSSConstants#FLAG_NO_GET}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSS#getGetLocalCopy()}
   */
  @Test
  public void testGetGetLocalCopy_thenReturnFlag_no_get() {
    // Arrange
    MSVSSADD msvssadd = new MSVSSADD();
    msvssadd.setInternalGetLocalCopy(false);

    // Act and Assert
    assertEquals(MSVSSConstants.FLAG_NO_GET, msvssadd.getGetLocalCopy());
  }

  /**
   * Test {@link MSVSS#getFileTimeStamp()}.
   * <ul>
   *   <li>Given {@link MSVSSADD} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSS#getFileTimeStamp()}
   */
  @Test
  public void testGetFileTimeStamp_givenMsvssadd_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new MSVSSADD()).getFileTimeStamp());
  }

  /**
   * Test WritableFiles {@link WritableFiles#getValues()}.
   * <p>
   * Method under test: {@link WritableFiles#getValues()}
   */
  @Test
  public void testWritableFilesGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(
        new String[]{MSVSSConstants.WRITABLE_REPLACE, MSVSSConstants.WRITABLE_SKIP, MSVSSConstants.WRITABLE_FAIL},
        (new WritableFiles()).getValues());
  }

  /**
   * Test WritableFiles new {@link WritableFiles} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link WritableFiles}
   */
  @Test
  public void testWritableFilesNewWritableFiles() {
    // Arrange and Act
    WritableFiles actualWritableFiles = new WritableFiles();

    // Assert
    assertNull(actualWritableFiles.getValue());
    assertEquals(-1, actualWritableFiles.getIndex());
  }
}
