package org.apache.tools.ant.types;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.Tar;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.junit.Test;

public class TarFileSetDiffblueTest {
  /**
   * Test {@link TarFileSet#TarFileSet()}.
   * <p>
   * Method under test: {@link TarFileSet#TarFileSet()}
   */
  @Test
  public void testNewTarFileSet() {
    // Arrange and Act
    TarFileSet actualTarFileSet = new TarFileSet();

    // Assert
    assertEquals("", actualTarFileSet.getFullpath());
    assertEquals("", actualTarFileSet.getPrefix());
    assertEquals("", actualTarFileSet.getGroup());
    assertEquals("", actualTarFileSet.getUserName());
    assertEquals("TarFileSet", actualTarFileSet.getDataTypeName());
    assertNull(actualTarFileSet.getDir());
    assertNull(actualTarFileSet.getSrc());
    assertNull(actualTarFileSet.getDescription());
    assertNull(actualTarFileSet.getEncoding());
    assertNull(actualTarFileSet.getProject());
    assertNull(actualTarFileSet.getRefid());
    assertEquals(0, actualTarFileSet.getGid());
    assertEquals(0, actualTarFileSet.getUid());
    assertEquals(5, actualTarFileSet.getMaxLevelsOfSymlinks());
    assertFalse(actualTarFileSet.isReference());
    assertFalse(actualTarFileSet.hasGroupBeenSet());
    assertFalse(actualTarFileSet.hasGroupIdBeenSet());
    assertFalse(actualTarFileSet.hasUserIdBeenSet());
    assertFalse(actualTarFileSet.hasUserNameBeenSet());
    assertTrue(actualTarFileSet.getDefaultexcludes());
    assertTrue(actualTarFileSet.getErrorOnMissingDir());
    assertTrue(actualTarFileSet.isFilesystemOnly());
    assertTrue(actualTarFileSet.isChecked());
    assertEquals(ArchiveFileSet.DEFAULT_DIR_MODE, actualTarFileSet.getDirMode());
    assertEquals(ArchiveFileSet.DEFAULT_FILE_MODE, actualTarFileSet.getFileMode());
  }

  /**
   * Test {@link TarFileSet#TarFileSet(FileSet)}.
   * <ul>
   *   <li>When {@link FileSet#FileSet()}.</li>
   *   <li>Then return Fullpath is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarFileSet#TarFileSet(FileSet)}
   */
  @Test
  public void testNewTarFileSet_whenFileSet_thenReturnFullpathIsEmptyString() {
    // Arrange and Act
    TarFileSet actualTarFileSet = new TarFileSet(new FileSet());

    // Assert
    assertEquals("", actualTarFileSet.getFullpath());
    assertEquals("", actualTarFileSet.getPrefix());
    assertEquals("", actualTarFileSet.getGroup());
    assertEquals("", actualTarFileSet.getUserName());
    assertEquals("TarFileSet", actualTarFileSet.getDataTypeName());
    assertNull(actualTarFileSet.getDir());
    assertNull(actualTarFileSet.getSrc());
    assertNull(actualTarFileSet.getDescription());
    assertNull(actualTarFileSet.getEncoding());
    assertNull(actualTarFileSet.getProject());
    assertNull(actualTarFileSet.getRefid());
    assertEquals(0, actualTarFileSet.getGid());
    assertEquals(0, actualTarFileSet.getUid());
    assertEquals(5, actualTarFileSet.getMaxLevelsOfSymlinks());
    assertFalse(actualTarFileSet.isReference());
    assertFalse(actualTarFileSet.hasGroupBeenSet());
    assertFalse(actualTarFileSet.hasGroupIdBeenSet());
    assertFalse(actualTarFileSet.hasUserIdBeenSet());
    assertFalse(actualTarFileSet.hasUserNameBeenSet());
    assertTrue(actualTarFileSet.getDefaultexcludes());
    assertTrue(actualTarFileSet.getErrorOnMissingDir());
    assertTrue(actualTarFileSet.isFilesystemOnly());
    assertTrue(actualTarFileSet.isChecked());
    assertEquals(ArchiveFileSet.DEFAULT_DIR_MODE, actualTarFileSet.getDirMode());
    assertEquals(ArchiveFileSet.DEFAULT_FILE_MODE, actualTarFileSet.getFileMode());
  }

  /**
   * Test {@link TarFileSet#TarFileSet(TarFileSet)}.
   * <ul>
   *   <li>When {@link TarFileSet#TarFileSet()}.</li>
   *   <li>Then return Fullpath is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarFileSet#TarFileSet(TarFileSet)}
   */
  @Test
  public void testNewTarFileSet_whenTarFileSet_thenReturnFullpathIsEmptyString() {
    // Arrange and Act
    TarFileSet actualTarFileSet = new TarFileSet(new TarFileSet());

    // Assert
    assertEquals("", actualTarFileSet.getFullpath());
    assertEquals("", actualTarFileSet.getPrefix());
    assertEquals("", actualTarFileSet.getGroup());
    assertEquals("", actualTarFileSet.getUserName());
    assertEquals("TarFileSet", actualTarFileSet.getDataTypeName());
    assertNull(actualTarFileSet.getDir());
    assertNull(actualTarFileSet.getSrc());
    assertNull(actualTarFileSet.getDescription());
    assertNull(actualTarFileSet.getEncoding());
    assertNull(actualTarFileSet.getProject());
    assertNull(actualTarFileSet.getRefid());
    assertEquals(0, actualTarFileSet.getGid());
    assertEquals(0, actualTarFileSet.getUid());
    assertEquals(5, actualTarFileSet.getMaxLevelsOfSymlinks());
    assertFalse(actualTarFileSet.isReference());
    assertFalse(actualTarFileSet.hasGroupBeenSet());
    assertFalse(actualTarFileSet.hasGroupIdBeenSet());
    assertFalse(actualTarFileSet.hasUserIdBeenSet());
    assertFalse(actualTarFileSet.hasUserNameBeenSet());
    assertTrue(actualTarFileSet.getDefaultexcludes());
    assertTrue(actualTarFileSet.getErrorOnMissingDir());
    assertTrue(actualTarFileSet.isFilesystemOnly());
    assertTrue(actualTarFileSet.isChecked());
    assertEquals(ArchiveFileSet.DEFAULT_DIR_MODE, actualTarFileSet.getDirMode());
    assertEquals(ArchiveFileSet.DEFAULT_FILE_MODE, actualTarFileSet.getFileMode());
  }

  /**
   * Test {@link TarFileSet#setUserName(String)}.
   * <ul>
   *   <li>Given {@link TarFileSet#TarFileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarFileSet#setUserName(String)}
   */
  @Test
  public void testSetUserName_givenTarFileSet() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act
    tarFileSet.setUserName("janedoe");

    // Assert
    assertEquals("janedoe", tarFileSet.getUserName());
    assertTrue(tarFileSet.hasUserNameBeenSet());
  }

  /**
   * Test {@link TarFileSet#setUserName(String)}.
   * <ul>
   *   <li>Given {@link TarFileSet#TarFileSet()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link TarFileSet#setUserName(String)}
   */
  @Test
  public void testSetUserName_givenTarFileSetProjectIsProject() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setProject(new Project());

    // Act
    tarFileSet.setUserName("janedoe");

    // Assert
    assertEquals("janedoe", tarFileSet.getUserName());
    assertTrue(tarFileSet.hasUserNameBeenSet());
  }

  /**
   * Test {@link TarFileSet#getUserName()}.
   * <p>
   * Method under test: {@link TarFileSet#getUserName()}
   */
  @Test
  public void testGetUserName() {
    // Arrange, Act and Assert
    assertEquals("", (new TarFileSet()).getUserName());
  }

  /**
   * Test {@link TarFileSet#setUid(int)}.
   * <ul>
   *   <li>Given {@link TarFileSet#TarFileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarFileSet#setUid(int)}
   */
  @Test
  public void testSetUid_givenTarFileSet() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act
    tarFileSet.setUid(1);

    // Assert
    assertEquals(1, tarFileSet.getUid());
    assertTrue(tarFileSet.hasUserIdBeenSet());
  }

  /**
   * Test {@link TarFileSet#setUid(int)}.
   * <ul>
   *   <li>Given {@link TarFileSet#TarFileSet()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link TarFileSet#setUid(int)}
   */
  @Test
  public void testSetUid_givenTarFileSetProjectIsProject() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setProject(new Project());

    // Act
    tarFileSet.setUid(1);

    // Assert
    assertEquals(1, tarFileSet.getUid());
    assertTrue(tarFileSet.hasUserIdBeenSet());
  }

  /**
   * Test {@link TarFileSet#getUid()}.
   * <p>
   * Method under test: {@link TarFileSet#getUid()}
   */
  @Test
  public void testGetUid() {
    // Arrange, Act and Assert
    assertEquals(0, (new TarFileSet()).getUid());
  }

  /**
   * Test {@link TarFileSet#setGroup(String)}.
   * <ul>
   *   <li>Given {@link TarFileSet#TarFileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarFileSet#setGroup(String)}
   */
  @Test
  public void testSetGroup_givenTarFileSet() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act
    tarFileSet.setGroup("Group Name");

    // Assert
    assertEquals("Group Name", tarFileSet.getGroup());
    assertTrue(tarFileSet.hasGroupBeenSet());
  }

  /**
   * Test {@link TarFileSet#setGroup(String)}.
   * <ul>
   *   <li>Given {@link TarFileSet#TarFileSet()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link TarFileSet#setGroup(String)}
   */
  @Test
  public void testSetGroup_givenTarFileSetProjectIsProject() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setProject(new Project());

    // Act
    tarFileSet.setGroup("Group Name");

    // Assert
    assertEquals("Group Name", tarFileSet.getGroup());
    assertTrue(tarFileSet.hasGroupBeenSet());
  }

  /**
   * Test {@link TarFileSet#getGroup()}.
   * <p>
   * Method under test: {@link TarFileSet#getGroup()}
   */
  @Test
  public void testGetGroup() {
    // Arrange, Act and Assert
    assertEquals("", (new TarFileSet()).getGroup());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link TarFileSet#hasGroupBeenSet()}
   *   <li>{@link TarFileSet#hasGroupIdBeenSet()}
   *   <li>{@link TarFileSet#hasUserIdBeenSet()}
   *   <li>{@link TarFileSet#hasUserNameBeenSet()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act
    boolean actualHasGroupBeenSetResult = tarFileSet.hasGroupBeenSet();
    boolean actualHasGroupIdBeenSetResult = tarFileSet.hasGroupIdBeenSet();
    boolean actualHasUserIdBeenSetResult = tarFileSet.hasUserIdBeenSet();

    // Assert
    assertFalse(actualHasGroupBeenSetResult);
    assertFalse(actualHasGroupIdBeenSetResult);
    assertFalse(actualHasUserIdBeenSetResult);
    assertFalse(tarFileSet.hasUserNameBeenSet());
  }

  /**
   * Test {@link TarFileSet#setGid(int)}.
   * <ul>
   *   <li>Given {@link TarFileSet#TarFileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarFileSet#setGid(int)}
   */
  @Test
  public void testSetGid_givenTarFileSet() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act
    tarFileSet.setGid(1);

    // Assert
    assertEquals(1, tarFileSet.getGid());
    assertTrue(tarFileSet.hasGroupIdBeenSet());
  }

  /**
   * Test {@link TarFileSet#setGid(int)}.
   * <ul>
   *   <li>Given {@link TarFileSet#TarFileSet()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link TarFileSet#setGid(int)}
   */
  @Test
  public void testSetGid_givenTarFileSetProjectIsProject() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setProject(new Project());

    // Act
    tarFileSet.setGid(1);

    // Assert
    assertEquals(1, tarFileSet.getGid());
    assertTrue(tarFileSet.hasGroupIdBeenSet());
  }

  /**
   * Test {@link TarFileSet#getGid()}.
   * <p>
   * Method under test: {@link TarFileSet#getGid()}
   */
  @Test
  public void testGetGid() {
    // Arrange, Act and Assert
    assertEquals(0, (new TarFileSet()).getGid());
  }

  /**
   * Test {@link TarFileSet#newArchiveScanner()}.
   * <p>
   * Method under test: {@link TarFileSet#newArchiveScanner()}
   */
  @Test
  public void testNewArchiveScanner() {
    // Arrange and Act
    ArchiveScanner actualNewArchiveScannerResult = (new TarFileSet()).newArchiveScanner();

    // Assert
    assertTrue(actualNewArchiveScannerResult instanceof TarScanner);
    assertNull(actualNewArchiveScannerResult.getBasedir());
    assertNull(((TarScanner) actualNewArchiveScannerResult).srcFile);
    assertEquals(0, actualNewArchiveScannerResult.getNotFollowedSymlinks().length);
    assertTrue(actualNewArchiveScannerResult.isCaseSensitive());
    assertTrue(actualNewArchiveScannerResult.isEverythingIncluded());
    assertTrue(actualNewArchiveScannerResult.isFollowSymlinks());
  }

  /**
   * Test {@link TarFileSet#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link TarFileSet#TarFileSet()}.</li>
   *   <li>Then not {@link TarFileSet#TarFileSet()} Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarFileSet#setRefid(Reference)}
   */
  @Test
  public void testSetRefid_givenTarFileSet_thenNotTarFileSetChecked() throws BuildException {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    Reference r = new Reference("42");

    // Act
    tarFileSet.setRefid(r);

    // Assert
    assertFalse(tarFileSet.isChecked());
    assertTrue(tarFileSet.isReference());
    assertSame(r, tarFileSet.getRefid());
  }

  /**
   * Test {@link TarFileSet#configureFileSet(ArchiveFileSet)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link Tar.TarFileSet#TarFileSet()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link TarFileSet#configureFileSet(ArchiveFileSet)}
   */
  @Test
  public void testConfigureFileSet_givenProject_whenTarFileSetProjectIsProject() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    Tar.TarFileSet zfs = new Tar.TarFileSet();
    zfs.setProject(new Project());

    // Act
    tarFileSet.configureFileSet(zfs);

    // Assert
    assertEquals("", zfs.getPrefix());
    assertTrue(zfs.hasGroupBeenSet());
    assertTrue(zfs.hasGroupIdBeenSet());
    assertTrue(zfs.hasUserIdBeenSet());
    assertTrue(zfs.hasUserNameBeenSet());
  }

  /**
   * Test {@link TarFileSet#configureFileSet(ArchiveFileSet)}.
   * <ul>
   *   <li>Given {@link TarFileSet#TarFileSet()} Prefix is {@code Prefix}.</li>
   *   <li>Then {@link Tar.TarFileSet#TarFileSet()} Prefix is {@code Prefix}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarFileSet#configureFileSet(ArchiveFileSet)}
   */
  @Test
  public void testConfigureFileSet_givenTarFileSetPrefixIsPrefix_thenTarFileSetPrefixIsPrefix() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setPrefix("Prefix");
    Tar.TarFileSet zfs = new Tar.TarFileSet();

    // Act
    tarFileSet.configureFileSet(zfs);

    // Assert
    assertEquals("Prefix", zfs.getPrefix());
    assertTrue(zfs.hasGroupBeenSet());
    assertTrue(zfs.hasGroupIdBeenSet());
    assertTrue(zfs.hasUserIdBeenSet());
    assertTrue(zfs.hasUserNameBeenSet());
  }

  /**
   * Test {@link TarFileSet#configureFileSet(ArchiveFileSet)}.
   * <ul>
   *   <li>When {@link Tar.TarFileSet#TarFileSet()}.</li>
   *   <li>Then {@link Tar.TarFileSet#TarFileSet()} Prefix is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarFileSet#configureFileSet(ArchiveFileSet)}
   */
  @Test
  public void testConfigureFileSet_whenTarFileSet_thenTarFileSetPrefixIsEmptyString() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    Tar.TarFileSet zfs = new Tar.TarFileSet();

    // Act
    tarFileSet.configureFileSet(zfs);

    // Assert
    assertEquals("", zfs.getPrefix());
    assertTrue(zfs.hasGroupBeenSet());
    assertTrue(zfs.hasGroupIdBeenSet());
    assertTrue(zfs.hasUserIdBeenSet());
    assertTrue(zfs.hasUserNameBeenSet());
  }

  /**
   * Test {@link TarFileSet#configureFileSet(ArchiveFileSet)}.
   * <ul>
   *   <li>When {@link ZipFileSet#ZipFileSet()}.</li>
   *   <li>Then {@link ZipFileSet#ZipFileSet()} Prefix is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarFileSet#configureFileSet(ArchiveFileSet)}
   */
  @Test
  public void testConfigureFileSet_whenZipFileSet_thenZipFileSetPrefixIsEmptyString() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    ZipFileSet zfs = new ZipFileSet();

    // Act
    tarFileSet.configureFileSet(zfs);

    // Assert that nothing has changed
    assertEquals("", zfs.getPrefix());
  }

  /**
   * Test {@link TarFileSet#clone()}.
   * <ul>
   *   <li>Given {@link TarFileSet#TarFileSet()} appendSelector {@link ScriptSelector} (default constructor).</li>
   *   <li>Then return not Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarFileSet#clone()}
   */
  @Test
  public void testClone_givenTarFileSetAppendSelectorScriptSelector_thenReturnNotChecked() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.appendSelector(new ScriptSelector());

    // Act
    Object actualCloneResult = tarFileSet.clone();

    // Assert
    assertTrue(actualCloneResult instanceof TarFileSet);
    assertEquals("", ((TarFileSet) actualCloneResult).getFullpath());
    assertEquals("", ((TarFileSet) actualCloneResult).getPrefix());
    assertEquals("", ((TarFileSet) actualCloneResult).getGroup());
    assertEquals("", ((TarFileSet) actualCloneResult).getUserName());
    assertEquals("TarFileSet", ((TarFileSet) actualCloneResult).getDataTypeName());
    assertNull(((TarFileSet) actualCloneResult).getDir());
    assertNull(((TarFileSet) actualCloneResult).getSrc());
    assertNull(((TarFileSet) actualCloneResult).getDescription());
    assertNull(((TarFileSet) actualCloneResult).getEncoding());
    assertNull(((TarFileSet) actualCloneResult).getProject());
    assertNull(((TarFileSet) actualCloneResult).getRefid());
    assertEquals(0, ((TarFileSet) actualCloneResult).getGid());
    assertEquals(0, ((TarFileSet) actualCloneResult).getUid());
    assertEquals(5, ((TarFileSet) actualCloneResult).getMaxLevelsOfSymlinks());
    assertFalse(((TarFileSet) actualCloneResult).isChecked());
    assertFalse(((TarFileSet) actualCloneResult).isReference());
    assertFalse(((TarFileSet) actualCloneResult).hasGroupBeenSet());
    assertFalse(((TarFileSet) actualCloneResult).hasGroupIdBeenSet());
    assertFalse(((TarFileSet) actualCloneResult).hasUserIdBeenSet());
    assertFalse(((TarFileSet) actualCloneResult).hasUserNameBeenSet());
    assertTrue(((TarFileSet) actualCloneResult).getDefaultexcludes());
    assertTrue(((TarFileSet) actualCloneResult).getErrorOnMissingDir());
    assertTrue(((TarFileSet) actualCloneResult).isFilesystemOnly());
    assertEquals(ArchiveFileSet.DEFAULT_DIR_MODE, ((TarFileSet) actualCloneResult).getDirMode());
    assertEquals(ArchiveFileSet.DEFAULT_FILE_MODE, ((TarFileSet) actualCloneResult).getFileMode());
  }

  /**
   * Test {@link TarFileSet#clone()}.
   * <ul>
   *   <li>Given {@link TarFileSet#TarFileSet()}.</li>
   *   <li>Then return Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarFileSet#clone()}
   */
  @Test
  public void testClone_givenTarFileSet_thenReturnChecked() {
    // Arrange and Act
    Object actualCloneResult = (new TarFileSet()).clone();

    // Assert
    assertTrue(actualCloneResult instanceof TarFileSet);
    assertEquals("", ((TarFileSet) actualCloneResult).getFullpath());
    assertEquals("", ((TarFileSet) actualCloneResult).getPrefix());
    assertEquals("", ((TarFileSet) actualCloneResult).getGroup());
    assertEquals("", ((TarFileSet) actualCloneResult).getUserName());
    assertEquals("TarFileSet", ((TarFileSet) actualCloneResult).getDataTypeName());
    assertNull(((TarFileSet) actualCloneResult).getDir());
    assertNull(((TarFileSet) actualCloneResult).getSrc());
    assertNull(((TarFileSet) actualCloneResult).getDescription());
    assertNull(((TarFileSet) actualCloneResult).getEncoding());
    assertNull(((TarFileSet) actualCloneResult).getProject());
    assertNull(((TarFileSet) actualCloneResult).getRefid());
    assertEquals(0, ((TarFileSet) actualCloneResult).getGid());
    assertEquals(0, ((TarFileSet) actualCloneResult).getUid());
    assertEquals(5, ((TarFileSet) actualCloneResult).getMaxLevelsOfSymlinks());
    assertFalse(((TarFileSet) actualCloneResult).isReference());
    assertFalse(((TarFileSet) actualCloneResult).hasGroupBeenSet());
    assertFalse(((TarFileSet) actualCloneResult).hasGroupIdBeenSet());
    assertFalse(((TarFileSet) actualCloneResult).hasUserIdBeenSet());
    assertFalse(((TarFileSet) actualCloneResult).hasUserNameBeenSet());
    assertTrue(((TarFileSet) actualCloneResult).getDefaultexcludes());
    assertTrue(((TarFileSet) actualCloneResult).getErrorOnMissingDir());
    assertTrue(((TarFileSet) actualCloneResult).isFilesystemOnly());
    assertTrue(((TarFileSet) actualCloneResult).isChecked());
    assertEquals(ArchiveFileSet.DEFAULT_DIR_MODE, ((TarFileSet) actualCloneResult).getDirMode());
    assertEquals(ArchiveFileSet.DEFAULT_FILE_MODE, ((TarFileSet) actualCloneResult).getFileMode());
  }
}
