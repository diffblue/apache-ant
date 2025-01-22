package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Paths;
import java.util.Hashtable;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.Definer.OnError;
import org.apache.tools.ant.taskdefs.Zip.ArchiveState;
import org.apache.tools.ant.taskdefs.Zip.Duplicate;
import org.apache.tools.ant.taskdefs.Zip.UnicodeExtraField;
import org.apache.tools.ant.taskdefs.Zip.WhenEmpty;
import org.apache.tools.ant.taskdefs.Zip.Zip64ModeAttribute;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileList.FileName;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.PropertySet;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.apache.tools.ant.types.resources.Resources;
import org.apache.tools.ant.types.resources.selectors.And;
import org.apache.tools.ant.types.resources.selectors.Exists;
import org.apache.tools.ant.types.resources.selectors.ResourceSelector;
import org.apache.tools.ant.types.resources.selectors.Type;
import org.apache.tools.zip.AsiExtraField;
import org.apache.tools.zip.JarMarker;
import org.apache.tools.zip.Zip64Mode;
import org.apache.tools.zip.ZipEntry;
import org.apache.tools.zip.ZipExtraField;
import org.apache.tools.zip.ZipOutputStream;
import org.junit.Test;

public class ZipDiffblueTest {
  /**
   * Test ArchiveState getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ArchiveState#ArchiveState(boolean, Resource[][])}
   *   <li>{@link ArchiveState#getResourcesToAdd()}
   *   <li>{@link ArchiveState#isOutOfDate()}
   * </ul>
   */
  @Test
  public void testArchiveStateGettersAndSetters() {
    // Arrange
    Resource[][] r = new Resource[][]{new Resource[]{new Resource()}};

    // Act
    ArchiveState actualArchiveState = new ArchiveState(true, r);
    Resource[][] actualResourcesToAdd = actualArchiveState.getResourcesToAdd();

    // Assert
    assertTrue(actualArchiveState.isOutOfDate());
    assertSame(r, actualResourcesToAdd);
  }

  /**
   * Test ArchiveState {@link ArchiveState#isWithoutAnyResources()}.
   * <p>
   * Method under test: {@link ArchiveState#isWithoutAnyResources()}
   */
  @Test
  public void testArchiveStateIsWithoutAnyResources() {
    // Arrange, Act and Assert
    assertTrue((new ArchiveState(true, new Resource[][]{null})).isWithoutAnyResources());
  }

  /**
   * Test ArchiveState {@link ArchiveState#isWithoutAnyResources()}.
   * <ul>
   *   <li>Given {@link ArchiveState#ArchiveState(boolean, Resource[][])} with state is {@code true} and r is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveState#isWithoutAnyResources()}
   */
  @Test
  public void testArchiveStateIsWithoutAnyResources_givenArchiveStateWithStateIsTrueAndRIsNull() {
    // Arrange, Act and Assert
    assertTrue((new ArchiveState(true, null)).isWithoutAnyResources());
  }

  /**
   * Test ArchiveState {@link ArchiveState#isWithoutAnyResources()}.
   * <ul>
   *   <li>Given empty array of {@link Resource}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveState#isWithoutAnyResources()}
   */
  @Test
  public void testArchiveStateIsWithoutAnyResources_givenEmptyArrayOfResource_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new ArchiveState(true, new Resource[][]{new Resource[]{}})).isWithoutAnyResources());
  }

  /**
   * Test ArchiveState {@link ArchiveState#isWithoutAnyResources()}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveState#isWithoutAnyResources()}
   */
  @Test
  public void testArchiveStateIsWithoutAnyResources_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new ArchiveState(true, new Resource[][]{new Resource[]{new Resource()}})).isWithoutAnyResources());
  }

  /**
   * Test Duplicate {@link Duplicate#getValues()}.
   * <p>
   * Method under test: {@link Duplicate#getValues()}
   */
  @Test
  public void testDuplicateGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"add", "preserve", OnError.POLICY_FAIL}, (new Duplicate()).getValues());
  }

  /**
   * Test Duplicate new {@link Duplicate} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Duplicate}
   */
  @Test
  public void testDuplicateNewDuplicate() {
    // Arrange and Act
    Duplicate actualDuplicate = new Duplicate();

    // Assert
    assertNull(actualDuplicate.getValue());
    assertEquals(-1, actualDuplicate.getIndex());
  }

  /**
   * Test {@link Zip#isFirstPass()}.
   * <p>
   * Method under test: {@link Zip#isFirstPass()}
   */
  @Test
  public void testIsFirstPass() {
    // Arrange, Act and Assert
    assertTrue((new Zip()).isFirstPass());
  }

  /**
   * Test {@link Zip#setZipfile(File)}.
   * <p>
   * Method under test: {@link Zip#setZipfile(File)}
   */
  @Test
  public void testSetZipfile() {
    // Arrange
    Zip zip = new Zip();
    File zipFile = Copy.NULL_FILE_PLACEHOLDER;

    // Act
    zip.setZipfile(zipFile);

    // Assert
    assertSame(zipFile, zip.getDestFile());
  }

  /**
   * Test {@link Zip#setFile(File)}.
   * <p>
   * Method under test: {@link Zip#setFile(File)}
   */
  @Test
  public void testSetFile() {
    // Arrange
    Zip zip = new Zip();
    File file = Copy.NULL_FILE_PLACEHOLDER;

    // Act
    zip.setFile(file);

    // Assert
    assertSame(file, zip.getDestFile());
  }

  /**
   * Test {@link Zip#setUpdate(boolean)}.
   * <p>
   * Method under test: {@link Zip#setUpdate(boolean)}
   */
  @Test
  public void testSetUpdate() {
    // Arrange
    Zip zip = new Zip();

    // Act
    zip.setUpdate(true);

    // Assert
    assertTrue(zip.isInUpdateMode());
  }

  /**
   * Test {@link Zip#setDuplicate(Duplicate)}.
   * <ul>
   *   <li>When {@link Duplicate} (default constructor).</li>
   *   <li>Then {@link Zip} (default constructor) {@link Zip#duplicate} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#setDuplicate(Duplicate)}
   */
  @Test
  public void testSetDuplicate_whenDuplicate_thenZipDuplicateIsNull() {
    // Arrange
    Zip zip = new Zip();

    // Act
    zip.setDuplicate(new Duplicate());

    // Assert
    assertNull(zip.duplicate);
  }

  /**
   * Test {@link Zip#setWhenempty(WhenEmpty)}.
   * <p>
   * Method under test: {@link Zip#setWhenempty(WhenEmpty)}
   */
  @Test
  public void testSetWhenempty() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new RecorderEntry("JARs are never empty, they contain at least a manifest file"));

    Ear ear = new Ear();
    ear.setProject(project);

    // Act
    ear.setWhenempty(new WhenEmpty());

    // Assert that nothing has changed
    assertEquals("create", ear.emptyBehavior);
  }

  /**
   * Test {@link Zip#setWhenempty(WhenEmpty)}.
   * <ul>
   *   <li>Given {@link Ear} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then {@link Ear} (default constructor) {@link Zip#emptyBehavior} is {@code create}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#setWhenempty(WhenEmpty)}
   */
  @Test
  public void testSetWhenempty_givenEarProjectIsProject_thenEarEmptyBehaviorIsCreate() {
    // Arrange
    Ear ear = new Ear();
    ear.setProject(new Project());

    // Act
    ear.setWhenempty(new WhenEmpty());

    // Assert that nothing has changed
    assertEquals("create", ear.emptyBehavior);
  }

  /**
   * Test {@link Zip#setWhenempty(WhenEmpty)}.
   * <ul>
   *   <li>Given {@link Ear} (default constructor).</li>
   *   <li>When {@link WhenEmpty} (default constructor).</li>
   *   <li>Then {@link Ear} (default constructor) {@link Zip#emptyBehavior} is {@code create}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#setWhenempty(WhenEmpty)}
   */
  @Test
  public void testSetWhenempty_givenEar_whenWhenEmpty_thenEarEmptyBehaviorIsCreate() {
    // Arrange
    Ear ear = new Ear();

    // Act
    ear.setWhenempty(new WhenEmpty());

    // Assert that nothing has changed
    assertEquals("create", ear.emptyBehavior);
  }

  /**
   * Test {@link Zip#setWhenempty(WhenEmpty)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@link WhenEmpty} (default constructor).</li>
   *   <li>Then {@link Ear} (default constructor) {@link Zip#emptyBehavior} is {@code create}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#setWhenempty(WhenEmpty)}
   */
  @Test
  public void testSetWhenempty_givenJavaLangObject_whenWhenEmpty_thenEarEmptyBehaviorIsCreate() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Type Name", typeClass);
    project.addBuildListener(new AntClassLoader());

    Ear ear = new Ear();
    ear.setProject(project);

    // Act
    ear.setWhenempty(new WhenEmpty());

    // Assert that nothing has changed
    assertEquals("create", ear.emptyBehavior);
  }

  /**
   * Test {@link Zip#setWhenempty(WhenEmpty)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#setWhenempty(WhenEmpty)}
   */
  @Test
  public void testSetWhenempty_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Ear ear = new Ear();
    ear.setProject(project);

    // Act
    ear.setWhenempty(new WhenEmpty());

    // Assert that nothing has changed
    assertEquals("create", ear.emptyBehavior);
  }

  /**
   * Test {@link Zip#setWhenempty(WhenEmpty)}.
   * <ul>
   *   <li>Given {@link Zip} (default constructor).</li>
   *   <li>When {@link WhenEmpty} (default constructor).</li>
   *   <li>Then {@link Zip} (default constructor) {@link Zip#emptyBehavior} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#setWhenempty(WhenEmpty)}
   */
  @Test
  public void testSetWhenempty_givenZip_whenWhenEmpty_thenZipEmptyBehaviorIsNull() {
    // Arrange
    Zip zip = new Zip();

    // Act
    zip.setWhenempty(new WhenEmpty());

    // Assert
    assertNull(zip.emptyBehavior);
  }

  /**
   * Test {@link Zip#execute()}.
   * <p>
   * Method under test: {@link Zip#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    Zip zip = new Zip();
    zip.setDestFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    zip.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> zip.execute());
  }

  /**
   * Test {@link Zip#execute()}.
   * <ul>
   *   <li>Given {@link Ear} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#execute()}
   */
  @Test
  public void testExecute_givenEar_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Ear()).execute());
  }

  /**
   * Test {@link Zip#execute()}.
   * <ul>
   *   <li>Given {@link Zip} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#execute()}
   */
  @Test
  public void testExecute_givenZipAddFilesetFileSet_thenThrowBuildException() throws BuildException {
    // Arrange
    Zip zip = new Zip();
    zip.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> zip.execute());
  }

  /**
   * Test {@link Zip#execute()}.
   * <ul>
   *   <li>Given {@link Zip} (default constructor) addZipGroupFileset {@link FileSet#FileSet()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#execute()}
   */
  @Test
  public void testExecute_givenZipAddZipGroupFilesetFileSet_thenThrowBuildException() throws BuildException {
    // Arrange
    Zip zip = new Zip();
    zip.addZipGroupFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> zip.execute());
  }

  /**
   * Test {@link Zip#execute()}.
   * <ul>
   *   <li>Given {@link Zip} (default constructor) Basedir is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#execute()}
   */
  @Test
  public void testExecute_givenZipBasedirIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    Zip zip = new Zip();
    zip.setBasedir(Copy.NULL_FILE_PLACEHOLDER);
    zip.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> zip.execute());
  }

  /**
   * Test {@link Zip#execute()}.
   * <ul>
   *   <li>Given {@link Zip} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#execute()}
   */
  @Test
  public void testExecute_givenZip_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Zip()).execute());
  }

  /**
   * Test {@link Zip#executeMain()}.
   * <p>
   * Method under test: {@link Zip#executeMain()}
   */
  @Test
  public void testExecuteMain() throws BuildException {
    // Arrange
    Zip zip = new Zip();
    zip.setDestFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    zip.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> zip.executeMain());
  }

  /**
   * Test {@link Zip#executeMain()}.
   * <ul>
   *   <li>Given {@link Ear} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#executeMain()}
   */
  @Test
  public void testExecuteMain_givenEar_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Ear()).executeMain());
  }

  /**
   * Test {@link Zip#executeMain()}.
   * <ul>
   *   <li>Given {@link Zip} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#executeMain()}
   */
  @Test
  public void testExecuteMain_givenZipAddFilesetFileSet_thenThrowBuildException() throws BuildException {
    // Arrange
    Zip zip = new Zip();
    zip.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> zip.executeMain());
  }

  /**
   * Test {@link Zip#executeMain()}.
   * <ul>
   *   <li>Given {@link Zip} (default constructor) addZipGroupFileset {@link FileSet#FileSet()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#executeMain()}
   */
  @Test
  public void testExecuteMain_givenZipAddZipGroupFilesetFileSet_thenThrowBuildException() throws BuildException {
    // Arrange
    Zip zip = new Zip();
    zip.addZipGroupFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> zip.executeMain());
  }

  /**
   * Test {@link Zip#executeMain()}.
   * <ul>
   *   <li>Given {@link Zip} (default constructor) Basedir is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#executeMain()}
   */
  @Test
  public void testExecuteMain_givenZipBasedirIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    Zip zip = new Zip();
    zip.setBasedir(Copy.NULL_FILE_PLACEHOLDER);
    zip.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> zip.executeMain());
  }

  /**
   * Test {@link Zip#executeMain()}.
   * <ul>
   *   <li>Given {@link Zip} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#executeMain()}
   */
  @Test
  public void testExecuteMain_givenZip_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Zip()).executeMain());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Zip#setBasedir(File)}
   *   <li>{@link Zip#setComment(String)}
   *   <li>{@link Zip#setCompress(boolean)}
   *   <li>{@link Zip#setCreateUnicodeExtraFields(UnicodeExtraField)}
   *   <li>{@link Zip#setDestFile(File)}
   *   <li>{@link Zip#setEncoding(String)}
   *   <li>{@link Zip#setFallBackToUTF8(boolean)}
   *   <li>{@link Zip#setFilesonly(boolean)}
   *   <li>{@link Zip#setKeepCompression(boolean)}
   *   <li>{@link Zip#setLevel(int)}
   *   <li>{@link Zip#setModificationtime(String)}
   *   <li>{@link Zip#setPreserve0Permissions(boolean)}
   *   <li>{@link Zip#setRoundUp(boolean)}
   *   <li>{@link Zip#setUseLanguageEncodingFlag(boolean)}
   *   <li>{@link Zip#setZip64Mode(Zip64ModeAttribute)}
   *   <li>{@link Zip#getComment()}
   *   <li>{@link Zip#getCreateUnicodeExtraFields()}
   *   <li>{@link Zip#getDestFile()}
   *   <li>{@link Zip#getEncoding()}
   *   <li>{@link Zip#getFallBackToUTF8()}
   *   <li>{@link Zip#getLevel()}
   *   <li>{@link Zip#getModificationtime()}
   *   <li>{@link Zip#getPreserve0Permissions()}
   *   <li>{@link Zip#getUseLanguageEnodingFlag()}
   *   <li>{@link Zip#getZip64Mode()}
   *   <li>{@link Zip#hasUpdatedFile()}
   *   <li>{@link Zip#isAddingNewFiles()}
   *   <li>{@link Zip#isCompress()}
   *   <li>{@link Zip#isInUpdateMode()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Zip zip = new Zip();

    // Act
    zip.setBasedir(Copy.NULL_FILE_PLACEHOLDER);
    zip.setComment("Comment");
    zip.setCompress(true);
    UnicodeExtraField b = new UnicodeExtraField();
    zip.setCreateUnicodeExtraFields(b);
    File destFile = Copy.NULL_FILE_PLACEHOLDER;
    zip.setDestFile(destFile);
    zip.setEncoding(Manifest.JAR_ENCODING);
    zip.setFallBackToUTF8(true);
    zip.setFilesonly(true);
    zip.setKeepCompression(true);
    zip.setLevel(1);
    zip.setModificationtime("Time");
    zip.setPreserve0Permissions(true);
    zip.setRoundUp(true);
    zip.setUseLanguageEncodingFlag(true);
    Zip64ModeAttribute b2 = new Zip64ModeAttribute();
    zip.setZip64Mode(b2);
    String actualComment = zip.getComment();
    UnicodeExtraField actualCreateUnicodeExtraFields = zip.getCreateUnicodeExtraFields();
    File actualDestFile = zip.getDestFile();
    String actualEncoding = zip.getEncoding();
    boolean actualFallBackToUTF8 = zip.getFallBackToUTF8();
    int actualLevel = zip.getLevel();
    String actualModificationtime = zip.getModificationtime();
    boolean actualPreserve0Permissions = zip.getPreserve0Permissions();
    boolean actualUseLanguageEnodingFlag = zip.getUseLanguageEnodingFlag();
    Zip64ModeAttribute actualZip64Mode = zip.getZip64Mode();
    boolean actualHasUpdatedFileResult = zip.hasUpdatedFile();
    boolean actualIsAddingNewFilesResult = zip.isAddingNewFiles();
    boolean actualIsCompressResult = zip.isCompress();

    // Assert
    assertEquals("Comment", actualComment);
    assertEquals("Time", actualModificationtime);
    assertEquals(1, actualLevel);
    assertFalse(actualHasUpdatedFileResult);
    assertFalse(actualIsAddingNewFilesResult);
    assertFalse(zip.isInUpdateMode());
    assertTrue(actualFallBackToUTF8);
    assertTrue(actualPreserve0Permissions);
    assertTrue(actualUseLanguageEnodingFlag);
    assertTrue(actualIsCompressResult);
    assertEquals(Manifest.JAR_ENCODING, actualEncoding);
    assertSame(b, actualCreateUnicodeExtraFields);
    assertSame(b2, actualZip64Mode);
    assertSame(destFile, actualDestFile);
  }

  /**
   * Test {@link Zip#createEmptyZip(File)}.
   * <ul>
   *   <li>Given {@link Ear} (default constructor).</li>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#createEmptyZip(File)}
   */
  @Test
  public void testCreateEmptyZip_givenEar_whenNull_file_placeholder_thenReturnTrue() throws BuildException {
    // Arrange, Act and Assert
    assertTrue((new Ear()).createEmptyZip(Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Zip#getResourcesToAdd(FileSet[], File, boolean)} with {@code filesets}, {@code zipFile}, {@code needsUpdate}.
   * <ul>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#getResourcesToAdd(FileSet[], File, boolean)}
   */
  @Test
  public void testGetResourcesToAddWithFilesetsZipFileNeedsUpdate_thenReturnArrayLengthIsZero() throws BuildException {
    // Arrange and Act
    ArchiveState actualResourcesToAdd = (new Zip()).getResourcesToAdd(new FileSet[]{}, Copy.NULL_FILE_PLACEHOLDER,
        true);

    // Assert
    assertEquals(0, actualResourcesToAdd.getResourcesToAdd().length);
    assertTrue(actualResourcesToAdd.isOutOfDate());
    assertTrue(actualResourcesToAdd.isWithoutAnyResources());
  }

  /**
   * Test {@link Zip#getResourcesToAdd(ResourceCollection[], File, boolean)} with {@code rcs}, {@code zipFile}, {@code needsUpdate}.
   * <p>
   * Method under test: {@link Zip#getResourcesToAdd(ResourceCollection[], File, boolean)}
   */
  @Test
  public void testGetResourcesToAddWithRcsZipFileNeedsUpdate() throws BuildException {
    // Arrange and Act
    ArchiveState actualResourcesToAdd = (new Zip()).getResourcesToAdd(new ResourceCollection[]{Resources.NONE},
        Copy.NULL_FILE_PLACEHOLDER, true);

    // Assert
    Resource[][] resourcesToAdd = actualResourcesToAdd.getResourcesToAdd();
    assertEquals(0, (resourcesToAdd[0]).length);
    assertEquals(1, resourcesToAdd.length);
    assertTrue(actualResourcesToAdd.isOutOfDate());
    assertTrue(actualResourcesToAdd.isWithoutAnyResources());
  }

  /**
   * Test {@link Zip#getResourcesToAdd(ResourceCollection[], File, boolean)} with {@code rcs}, {@code zipFile}, {@code needsUpdate}.
   * <ul>
   *   <li>Then return OutOfDate.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#getResourcesToAdd(ResourceCollection[], File, boolean)}
   */
  @Test
  public void testGetResourcesToAddWithRcsZipFileNeedsUpdate_thenReturnOutOfDate() throws BuildException {
    // Arrange
    Zip zip = new Zip();

    // Act
    ArchiveState actualResourcesToAdd = zip.getResourcesToAdd(new ResourceCollection[]{new FileList()},
        Copy.NULL_FILE_PLACEHOLDER, true);

    // Assert
    Resource[][] resourcesToAdd = actualResourcesToAdd.getResourcesToAdd();
    assertEquals(0, (resourcesToAdd[0]).length);
    assertEquals(1, resourcesToAdd.length);
    assertTrue(actualResourcesToAdd.isOutOfDate());
    assertTrue(actualResourcesToAdd.isWithoutAnyResources());
  }

  /**
   * Test {@link Zip#getResourcesToAdd(ResourceCollection[], File, boolean)} with {@code rcs}, {@code zipFile}, {@code needsUpdate}.
   * <ul>
   *   <li>Then return OutOfDate.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#getResourcesToAdd(ResourceCollection[], File, boolean)}
   */
  @Test
  public void testGetResourcesToAddWithRcsZipFileNeedsUpdate_thenReturnOutOfDate2() throws BuildException {
    // Arrange
    Zip zip = new Zip();

    FileName name = new FileName();
    name.setName("Warning: skipping ");

    FileList fileList = new FileList();
    fileList.addConfiguredFile(name);

    // Act
    ArchiveState actualResourcesToAdd = zip.getResourcesToAdd(new ResourceCollection[]{fileList},
        Copy.NULL_FILE_PLACEHOLDER, true);

    // Assert
    Resource[][] resourcesToAdd = actualResourcesToAdd.getResourcesToAdd();
    assertEquals(0, (resourcesToAdd[0]).length);
    assertEquals(1, resourcesToAdd.length);
    assertTrue(actualResourcesToAdd.isOutOfDate());
    assertTrue(actualResourcesToAdd.isWithoutAnyResources());
  }

  /**
   * Test {@link Zip#getResourcesToAdd(ResourceCollection[], File, boolean)} with {@code rcs}, {@code zipFile}, {@code needsUpdate}.
   * <ul>
   *   <li>When {@code false}.</li>
   *   <li>Then return not OutOfDate.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#getResourcesToAdd(ResourceCollection[], File, boolean)}
   */
  @Test
  public void testGetResourcesToAddWithRcsZipFileNeedsUpdate_whenFalse_thenReturnNotOutOfDate() throws BuildException {
    // Arrange and Act
    ArchiveState actualResourcesToAdd = (new Zip()).getResourcesToAdd(new ResourceCollection[]{Resources.NONE},
        Copy.NULL_FILE_PLACEHOLDER, false);

    // Assert
    Resource[][] resourcesToAdd = actualResourcesToAdd.getResourcesToAdd();
    assertEquals(0, (resourcesToAdd[0]).length);
    assertEquals(1, resourcesToAdd.length);
    assertFalse(actualResourcesToAdd.isOutOfDate());
    assertTrue(actualResourcesToAdd.isWithoutAnyResources());
  }

  /**
   * Test {@link Zip#getNonFileSetResourcesToAdd(ResourceCollection[], File, boolean)}.
   * <p>
   * Method under test: {@link Zip#getNonFileSetResourcesToAdd(ResourceCollection[], File, boolean)}
   */
  @Test
  public void testGetNonFileSetResourcesToAdd() throws BuildException {
    // Arrange
    Zip zip = new Zip();

    // Act
    ArchiveState actualNonFileSetResourcesToAdd = zip.getNonFileSetResourcesToAdd(
        new ResourceCollection[]{new Path(new Project(), "Path")}, Copy.NULL_FILE_PLACEHOLDER, true);

    // Assert
    Resource[][] resourcesToAdd = actualNonFileSetResourcesToAdd.getResourcesToAdd();
    assertEquals(0, (resourcesToAdd[0]).length);
    assertEquals(1, resourcesToAdd.length);
    assertTrue(actualNonFileSetResourcesToAdd.isOutOfDate());
    assertTrue(actualNonFileSetResourcesToAdd.isWithoutAnyResources());
  }

  /**
   * Test {@link Zip#getNonFileSetResourcesToAdd(ResourceCollection[], File, boolean)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#getNonFileSetResourcesToAdd(ResourceCollection[], File, boolean)}
   */
  @Test
  public void testGetNonFileSetResourcesToAdd_givenFileNameNameIs42() throws BuildException {
    // Arrange
    Zip zip = new Zip();

    FileName name = new FileName();
    name.setName("42");

    FileList fileList = new FileList();
    fileList.addConfiguredFile(name);

    // Act
    ArchiveState actualNonFileSetResourcesToAdd = zip.getNonFileSetResourcesToAdd(new ResourceCollection[]{fileList},
        Copy.NULL_FILE_PLACEHOLDER, true);

    // Assert
    Resource[][] resourcesToAdd = actualNonFileSetResourcesToAdd.getResourcesToAdd();
    assertEquals(0, (resourcesToAdd[0]).length);
    assertEquals(1, resourcesToAdd.length);
    assertTrue(actualNonFileSetResourcesToAdd.isOutOfDate());
    assertTrue(actualNonFileSetResourcesToAdd.isWithoutAnyResources());
  }

  /**
   * Test {@link Zip#getNonFileSetResourcesToAdd(ResourceCollection[], File, boolean)}.
   * <ul>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#getNonFileSetResourcesToAdd(ResourceCollection[], File, boolean)}
   */
  @Test
  public void testGetNonFileSetResourcesToAdd_thenReturnArrayLengthIsZero() throws BuildException {
    // Arrange
    Zip zip = new Zip();

    // Act
    ArchiveState actualNonFileSetResourcesToAdd = zip
        .getNonFileSetResourcesToAdd(new ResourceCollection[]{new FileList()}, Copy.NULL_FILE_PLACEHOLDER, true);

    // Assert
    Resource[][] resourcesToAdd = actualNonFileSetResourcesToAdd.getResourcesToAdd();
    assertEquals(0, (resourcesToAdd[0]).length);
    assertEquals(1, resourcesToAdd.length);
    assertTrue(actualNonFileSetResourcesToAdd.isOutOfDate());
    assertTrue(actualNonFileSetResourcesToAdd.isWithoutAnyResources());
  }

  /**
   * Test {@link Zip#getNonFileSetResourcesToAdd(ResourceCollection[], File, boolean)}.
   * <ul>
   *   <li>When array of {@link ResourceCollection} with {@link Resources#NONE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#getNonFileSetResourcesToAdd(ResourceCollection[], File, boolean)}
   */
  @Test
  public void testGetNonFileSetResourcesToAdd_whenArrayOfResourceCollectionWithNone() throws BuildException {
    // Arrange and Act
    ArchiveState actualNonFileSetResourcesToAdd = (new Zip())
        .getNonFileSetResourcesToAdd(new ResourceCollection[]{Resources.NONE}, Copy.NULL_FILE_PLACEHOLDER, true);

    // Assert
    Resource[][] resourcesToAdd = actualNonFileSetResourcesToAdd.getResourcesToAdd();
    assertEquals(0, (resourcesToAdd[0]).length);
    assertEquals(1, resourcesToAdd.length);
    assertTrue(actualNonFileSetResourcesToAdd.isOutOfDate());
    assertTrue(actualNonFileSetResourcesToAdd.isWithoutAnyResources());
  }

  /**
   * Test {@link Zip#getNonFileSetResourcesToAdd(ResourceCollection[], File, boolean)}.
   * <ul>
   *   <li>When array of {@link ResourceCollection} with {@link PropertySet} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#getNonFileSetResourcesToAdd(ResourceCollection[], File, boolean)}
   */
  @Test
  public void testGetNonFileSetResourcesToAdd_whenArrayOfResourceCollectionWithPropertySet() throws BuildException {
    // Arrange
    Zip zip = new Zip();

    // Act
    ArchiveState actualNonFileSetResourcesToAdd = zip
        .getNonFileSetResourcesToAdd(new ResourceCollection[]{new PropertySet()}, Copy.NULL_FILE_PLACEHOLDER, true);

    // Assert
    Resource[][] resourcesToAdd = actualNonFileSetResourcesToAdd.getResourcesToAdd();
    assertEquals(0, (resourcesToAdd[0]).length);
    assertEquals(1, resourcesToAdd.length);
    assertTrue(actualNonFileSetResourcesToAdd.isOutOfDate());
    assertTrue(actualNonFileSetResourcesToAdd.isWithoutAnyResources());
  }

  /**
   * Test {@link Zip#grabResources(FileSet[])}.
   * <ul>
   *   <li>Given {@link Zip} (default constructor).</li>
   *   <li>When empty array of {@link FileSet}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#grabResources(FileSet[])}
   */
  @Test
  public void testGrabResources_givenZip_whenEmptyArrayOfFileSet_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new Zip()).grabResources(new FileSet[]{}).length);
  }

  /**
   * Test {@link Zip#grabNonFileSetResources(ResourceCollection[])}.
   * <p>
   * Method under test: {@link Zip#grabNonFileSetResources(ResourceCollection[])}
   */
  @Test
  public void testGrabNonFileSetResources() {
    // Arrange
    Zip zip = new Zip();

    // Act
    Resource[][] actualGrabNonFileSetResourcesResult = zip
        .grabNonFileSetResources(new ResourceCollection[]{new Path(new Project(), "Path")});

    // Assert
    assertEquals(0, (actualGrabNonFileSetResourcesResult[0]).length);
    assertEquals(1, actualGrabNonFileSetResourcesResult.length);
  }

  /**
   * Test {@link Zip#grabNonFileSetResources(ResourceCollection[])}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code 42}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#grabNonFileSetResources(ResourceCollection[])}
   */
  @Test
  public void testGrabNonFileSetResources_givenFileNameNameIs42_thenReturnArrayLengthIsZero() {
    // Arrange
    Zip zip = new Zip();

    FileName name = new FileName();
    name.setName("42");

    FileList fileList = new FileList();
    fileList.addConfiguredFile(name);

    // Act
    Resource[][] actualGrabNonFileSetResourcesResult = zip.grabNonFileSetResources(new ResourceCollection[]{fileList});

    // Assert
    assertEquals(0, (actualGrabNonFileSetResourcesResult[0]).length);
    assertEquals(1, actualGrabNonFileSetResourcesResult.length);
  }

  /**
   * Test {@link Zip#grabNonFileSetResources(ResourceCollection[])}.
   * <ul>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#grabNonFileSetResources(ResourceCollection[])}
   */
  @Test
  public void testGrabNonFileSetResources_thenReturnArrayLengthIsZero() {
    // Arrange
    Zip zip = new Zip();

    // Act
    Resource[][] actualGrabNonFileSetResourcesResult = zip
        .grabNonFileSetResources(new ResourceCollection[]{new FileList()});

    // Assert
    assertEquals(0, (actualGrabNonFileSetResourcesResult[0]).length);
    assertEquals(1, actualGrabNonFileSetResourcesResult.length);
  }

  /**
   * Test {@link Zip#grabNonFileSetResources(ResourceCollection[])}.
   * <ul>
   *   <li>When array of {@link ResourceCollection} with {@link Resources#NONE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#grabNonFileSetResources(ResourceCollection[])}
   */
  @Test
  public void testGrabNonFileSetResources_whenArrayOfResourceCollectionWithNone() {
    // Arrange and Act
    Resource[][] actualGrabNonFileSetResourcesResult = (new Zip())
        .grabNonFileSetResources(new ResourceCollection[]{Resources.NONE});

    // Assert
    assertEquals(0, (actualGrabNonFileSetResourcesResult[0]).length);
    assertEquals(1, actualGrabNonFileSetResourcesResult.length);
  }

  /**
   * Test {@link Zip#grabNonFileSetResources(ResourceCollection[])}.
   * <ul>
   *   <li>When array of {@link ResourceCollection} with {@link PropertySet} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#grabNonFileSetResources(ResourceCollection[])}
   */
  @Test
  public void testGrabNonFileSetResources_whenArrayOfResourceCollectionWithPropertySet() {
    // Arrange
    Zip zip = new Zip();

    // Act
    Resource[][] actualGrabNonFileSetResourcesResult = zip
        .grabNonFileSetResources(new ResourceCollection[]{new PropertySet()});

    // Assert
    assertEquals(0, (actualGrabNonFileSetResourcesResult[0]).length);
    assertEquals(1, actualGrabNonFileSetResourcesResult.length);
  }

  /**
   * Test UnicodeExtraField {@link UnicodeExtraField#getPolicy()}.
   * <p>
   * Method under test: {@link UnicodeExtraField#getPolicy()}
   */
  @Test
  public void testUnicodeExtraFieldGetPolicy() {
    // Arrange, Act and Assert
    assertNull((new UnicodeExtraField()).getPolicy());
  }

  /**
   * Test UnicodeExtraField {@link UnicodeExtraField#getValues()}.
   * <p>
   * Method under test: {@link UnicodeExtraField#getValues()}
   */
  @Test
  public void testUnicodeExtraFieldGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"never", "always", "not-encodeable"}, (new UnicodeExtraField()).getValues());
  }

  /**
   * Test UnicodeExtraField {@link UnicodeExtraField#UnicodeExtraField()}.
   * <p>
   * Method under test: {@link UnicodeExtraField#UnicodeExtraField()}
   */
  @Test
  public void testUnicodeExtraFieldNewUnicodeExtraField() {
    // Arrange and Act
    UnicodeExtraField actualUnicodeExtraField = new UnicodeExtraField();

    // Assert
    assertNull(actualUnicodeExtraField.getValue());
    assertEquals(-1, actualUnicodeExtraField.getIndex());
  }

  /**
   * Test WhenEmpty {@link WhenEmpty#getValues()}.
   * <p>
   * Method under test: {@link WhenEmpty#getValues()}
   */
  @Test
  public void testWhenEmptyGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{OnError.POLICY_FAIL, "skip", "create"}, (new WhenEmpty()).getValues());
  }

  /**
   * Test WhenEmpty new {@link WhenEmpty} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link WhenEmpty}
   */
  @Test
  public void testWhenEmptyNewWhenEmpty() {
    // Arrange and Act
    WhenEmpty actualWhenEmpty = new WhenEmpty();

    // Assert
    assertNull(actualWhenEmpty.getValue());
    assertEquals(-1, actualWhenEmpty.getIndex());
  }

  /**
   * Test Zip64ModeAttribute {@link Zip64ModeAttribute#getMode()}.
   * <p>
   * Method under test: {@link Zip64ModeAttribute#getMode()}
   */
  @Test
  public void testZip64ModeAttributeGetMode() {
    // Arrange, Act and Assert
    assertNull((new Zip64ModeAttribute()).getMode());
  }

  /**
   * Test Zip64ModeAttribute {@link Zip64ModeAttribute#getValues()}.
   * <p>
   * Method under test: {@link Zip64ModeAttribute#getValues()}
   */
  @Test
  public void testZip64ModeAttributeGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"never", "always", "as-needed"}, (new Zip64ModeAttribute()).getValues());
  }

  /**
   * Test Zip64ModeAttribute {@link Zip64ModeAttribute#Zip64ModeAttribute()}.
   * <p>
   * Method under test: {@link Zip64ModeAttribute#Zip64ModeAttribute()}
   */
  @Test
  public void testZip64ModeAttributeNewZip64ModeAttribute() {
    // Arrange and Act
    Zip64ModeAttribute actualZip64ModeAttribute = new Zip64ModeAttribute();

    // Assert
    assertNull(actualZip64ModeAttribute.getValue());
    assertEquals(-1, actualZip64ModeAttribute.getIndex());
  }

  /**
   * Test {@link Zip#zipDir(File, ZipOutputStream, String, int)} with {@code File}, {@code ZipOutputStream}, {@code String}, {@code int}.
   * <p>
   * Method under test: {@link Zip#zipDir(File, ZipOutputStream, String, int)}
   */
  @Test
  public void testZipDirWithFileZipOutputStreamStringInt() throws IOException {
    // Arrange
    Zip zip = new Zip();

    // Act
    zip.zipDir(Copy.NULL_FILE_PLACEHOLDER, new ZipOutputStream(new ByteArrayOutputStream(1)), "V Path", 1);

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(File, ZipOutputStream, String, int)} with {@code File}, {@code ZipOutputStream}, {@code String}, {@code int}.
   * <p>
   * Method under test: {@link Zip#zipDir(File, ZipOutputStream, String, int)}
   */
  @Test
  public void testZipDirWithFileZipOutputStreamStringInt2() throws IOException {
    // Arrange
    Zip zip = new Zip();

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.putNextEntry(new ZipEntry("adding directory "));

    // Act
    zip.zipDir(Copy.NULL_FILE_PLACEHOLDER, zOut, "V Path", 1);

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(File, ZipOutputStream, String, int)} with {@code File}, {@code ZipOutputStream}, {@code String}, {@code int}.
   * <p>
   * Method under test: {@link Zip#zipDir(File, ZipOutputStream, String, int)}
   */
  @Test
  public void testZipDirWithFileZipOutputStreamStringInt3() throws IOException {
    // Arrange
    Zip zip = new Zip();
    zip.setModificationtime("adding directory ");

    // Act
    zip.zipDir(Copy.NULL_FILE_PLACEHOLDER, new ZipOutputStream(new ByteArrayOutputStream(1)), "V Path", 1);

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(File, ZipOutputStream, String, int, ZipExtraField[])} with {@code File}, {@code ZipOutputStream}, {@code String}, {@code int}, {@code ZipExtraField[]}.
   * <p>
   * Method under test: {@link Zip#zipDir(File, ZipOutputStream, String, int, ZipExtraField[])}
   */
  @Test
  public void testZipDirWithFileZipOutputStreamStringIntZipExtraField() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));

    // Act
    zip.zipDir(Copy.NULL_FILE_PLACEHOLDER, zOut, "V Path", 1, new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(File, ZipOutputStream, String, int, ZipExtraField[])} with {@code File}, {@code ZipOutputStream}, {@code String}, {@code int}, {@code ZipExtraField[]}.
   * <p>
   * Method under test: {@link Zip#zipDir(File, ZipOutputStream, String, int, ZipExtraField[])}
   */
  @Test
  public void testZipDirWithFileZipOutputStreamStringIntZipExtraField2() throws IOException {
    // Arrange
    Zip zip = new Zip();

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.putNextEntry(new ZipEntry("adding directory "));

    // Act
    zip.zipDir(Copy.NULL_FILE_PLACEHOLDER, zOut, "V Path", 1, new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(File, ZipOutputStream, String, int, ZipExtraField[])} with {@code File}, {@code ZipOutputStream}, {@code String}, {@code int}, {@code ZipExtraField[]}.
   * <p>
   * Method under test: {@link Zip#zipDir(File, ZipOutputStream, String, int, ZipExtraField[])}
   */
  @Test
  public void testZipDirWithFileZipOutputStreamStringIntZipExtraField3() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));

    // Act
    zip.zipDir(Copy.NULL_FILE_PLACEHOLDER, zOut, "/", 1, new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("/", stringStringMap.get("/"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(File, ZipOutputStream, String, int, ZipExtraField[])} with {@code File}, {@code ZipOutputStream}, {@code String}, {@code int}, {@code ZipExtraField[]}.
   * <p>
   * Method under test: {@link Zip#zipDir(File, ZipOutputStream, String, int, ZipExtraField[])}
   */
  @Test
  public void testZipDirWithFileZipOutputStreamStringIntZipExtraField4() throws IOException {
    // Arrange
    Zip zip = new Zip();

    // Act
    zip.zipDir(Copy.NULL_FILE_PLACEHOLDER, new ZipOutputStream(new ByteArrayOutputStream(1)), "V Path", 1, null);

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(File, ZipOutputStream, String, int, ZipExtraField[])} with {@code File}, {@code ZipOutputStream}, {@code String}, {@code int}, {@code ZipExtraField[]}.
   * <p>
   * Method under test: {@link Zip#zipDir(File, ZipOutputStream, String, int, ZipExtraField[])}
   */
  @Test
  public void testZipDirWithFileZipOutputStreamStringIntZipExtraField5() throws IOException {
    // Arrange
    Zip zip = new Zip();
    zip.setModificationtime("adding directory ");
    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));

    // Act
    zip.zipDir(Copy.NULL_FILE_PLACEHOLDER, zOut, "V Path", 1, new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(File, ZipOutputStream, String, int, ZipExtraField[])} with {@code File}, {@code ZipOutputStream}, {@code String}, {@code int}, {@code ZipExtraField[]}.
   * <p>
   * Method under test: {@link Zip#zipDir(File, ZipOutputStream, String, int, ZipExtraField[])}
   */
  @Test
  public void testZipDirWithFileZipOutputStreamStringIntZipExtraField6() throws IOException {
    // Arrange
    Zip zip = new Zip();

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.putNextEntry(new ZipEntry("adding directory "));

    // Act
    zip.zipDir(Copy.NULL_FILE_PLACEHOLDER, zOut, "adding directory ", 1, new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("adding directory ", stringStringMap.get("adding directory "));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(File, ZipOutputStream, String, int, ZipExtraField[])} with {@code File}, {@code ZipOutputStream}, {@code String}, {@code int}, {@code ZipExtraField[]}.
   * <ul>
   *   <li>Given {@code Always}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipDir(File, ZipOutputStream, String, int, ZipExtraField[])}
   */
  @Test
  public void testZipDirWithFileZipOutputStreamStringIntZipExtraField_givenAlways() throws IOException {
    // Arrange
    Zip zip = new Zip();

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.setUseZip64(Zip64Mode.Always);
    zOut.putNextEntry(new ZipEntry("adding directory "));

    // Act
    zip.zipDir(Copy.NULL_FILE_PLACEHOLDER, zOut, "V Path", 1, new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(File, ZipOutputStream, String, int, ZipExtraField[])} with {@code File}, {@code ZipOutputStream}, {@code String}, {@code int}, {@code ZipExtraField[]}.
   * <ul>
   *   <li>Given {@code Always}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipDir(File, ZipOutputStream, String, int, ZipExtraField[])}
   */
  @Test
  public void testZipDirWithFileZipOutputStreamStringIntZipExtraField_givenAlways2() throws IOException {
    // Arrange
    Zip zip = new Zip();

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.setUseZip64(Zip64Mode.Always);

    // Act
    zip.zipDir(Copy.NULL_FILE_PLACEHOLDER, zOut, "V Path", 1, new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(File, ZipOutputStream, String, int, ZipExtraField[])} with {@code File}, {@code ZipOutputStream}, {@code String}, {@code int}, {@code ZipExtraField[]}.
   * <ul>
   *   <li>Given {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipDir(File, ZipOutputStream, String, int, ZipExtraField[])}
   */
  @Test
  public void testZipDirWithFileZipOutputStreamStringIntZipExtraField_givenNull() throws IOException {
    // Arrange
    Zip zip = new Zip();

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.setCreateUnicodeExtraFields(null);
    zOut.putNextEntry(new ZipEntry("adding directory "));

    // Act
    zip.zipDir(Copy.NULL_FILE_PLACEHOLDER, zOut, "V Path", 1, new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(File, ZipOutputStream, String, int, ZipExtraField[])} with {@code File}, {@code ZipOutputStream}, {@code String}, {@code int}, {@code ZipExtraField[]}.
   * <ul>
   *   <li>Then {@link Zip} (default constructor) {@link Zip#addedDirs} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipDir(File, ZipOutputStream, String, int, ZipExtraField[])}
   */
  @Test
  public void testZipDirWithFileZipOutputStreamStringIntZipExtraField_thenZipAddedDirsEmpty() throws IOException {
    // Arrange
    Zip zip = new Zip();
    zip.setFilesonly(true);

    // Act
    zip.zipDir(Copy.NULL_FILE_PLACEHOLDER, null, "V Path", 1, new ZipExtraField[]{new AsiExtraField()});

    // Assert that nothing has changed
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
    assertTrue(zip.addedDirs.isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(File, ZipOutputStream, String, int, ZipExtraField[])} with {@code File}, {@code ZipOutputStream}, {@code String}, {@code int}, {@code ZipExtraField[]}.
   * <ul>
   *   <li>When minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipDir(File, ZipOutputStream, String, int, ZipExtraField[])}
   */
  @Test
  public void testZipDirWithFileZipOutputStreamStringIntZipExtraField_whenMinusOne() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));

    // Act
    zip.zipDir(Copy.NULL_FILE_PLACEHOLDER, zOut, "V Path", -1, new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(File, ZipOutputStream, String, int)} with {@code File}, {@code ZipOutputStream}, {@code String}, {@code int}.
   * <ul>
   *   <li>Given {@code Always}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipDir(File, ZipOutputStream, String, int)}
   */
  @Test
  public void testZipDirWithFileZipOutputStreamStringInt_givenAlways() throws IOException {
    // Arrange
    Zip zip = new Zip();

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.setUseZip64(Zip64Mode.Always);
    zOut.putNextEntry(new ZipEntry("adding directory "));

    // Act
    zip.zipDir(Copy.NULL_FILE_PLACEHOLDER, zOut, "V Path", 1);

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(File, ZipOutputStream, String, int)} with {@code File}, {@code ZipOutputStream}, {@code String}, {@code int}.
   * <ul>
   *   <li>Given {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipDir(File, ZipOutputStream, String, int)}
   */
  @Test
  public void testZipDirWithFileZipOutputStreamStringInt_givenNull() throws IOException {
    // Arrange
    Zip zip = new Zip();

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.setCreateUnicodeExtraFields(null);
    zOut.putNextEntry(new ZipEntry("adding directory "));

    // Act
    zip.zipDir(Copy.NULL_FILE_PLACEHOLDER, zOut, "V Path", 1);

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(File, ZipOutputStream, String, int)} with {@code File}, {@code ZipOutputStream}, {@code String}, {@code int}.
   * <ul>
   *   <li>Then {@link Zip} (default constructor) {@link Zip#addedDirs} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipDir(File, ZipOutputStream, String, int)}
   */
  @Test
  public void testZipDirWithFileZipOutputStreamStringInt_thenZipAddedDirsEmpty() throws IOException {
    // Arrange
    Zip zip = new Zip();
    zip.setFilesonly(true);

    // Act
    zip.zipDir(Copy.NULL_FILE_PLACEHOLDER, null, "V Path", 1);

    // Assert that nothing has changed
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
    assertTrue(zip.addedDirs.isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(File, ZipOutputStream, String, int)} with {@code File}, {@code ZipOutputStream}, {@code String}, {@code int}.
   * <ul>
   *   <li>Then {@link Zip} (default constructor) {@link Zip#addedDirs} {@code /} is {@code /}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipDir(File, ZipOutputStream, String, int)}
   */
  @Test
  public void testZipDirWithFileZipOutputStreamStringInt_thenZipAddedDirsSlashIsSlash() throws IOException {
    // Arrange
    Zip zip = new Zip();

    // Act
    zip.zipDir(Copy.NULL_FILE_PLACEHOLDER, new ZipOutputStream(new ByteArrayOutputStream(1)), "/", 1);

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("/", stringStringMap.get("/"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(File, ZipOutputStream, String, int)} with {@code File}, {@code ZipOutputStream}, {@code String}, {@code int}.
   * <ul>
   *   <li>When minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipDir(File, ZipOutputStream, String, int)}
   */
  @Test
  public void testZipDirWithFileZipOutputStreamStringInt_whenMinusOne() throws IOException {
    // Arrange
    Zip zip = new Zip();

    // Act
    zip.zipDir(Copy.NULL_FILE_PLACEHOLDER, new ZipOutputStream(new ByteArrayOutputStream(1)), "V Path", -1);

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(Resource, ZipOutputStream, String, int, ZipExtraField[])} with {@code Resource}, {@code ZipOutputStream}, {@code String}, {@code int}, {@code ZipExtraField[]}.
   * <p>
   * Method under test: {@link Zip#zipDir(Resource, ZipOutputStream, String, int, ZipExtraField[])}
   */
  @Test
  public void testZipDirWithResourceZipOutputStreamStringIntZipExtraField() throws IOException {
    // Arrange
    Zip zip = new Zip();
    Resource dir = new Resource();
    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));

    // Act
    zip.zipDir(dir, zOut, "V Path", 1, new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(Resource, ZipOutputStream, String, int, ZipExtraField[])} with {@code Resource}, {@code ZipOutputStream}, {@code String}, {@code int}, {@code ZipExtraField[]}.
   * <p>
   * Method under test: {@link Zip#zipDir(Resource, ZipOutputStream, String, int, ZipExtraField[])}
   */
  @Test
  public void testZipDirWithResourceZipOutputStreamStringIntZipExtraField2() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));

    // Act
    zip.zipDir((Resource) null, zOut, "V Path", 1, new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(Resource, ZipOutputStream, String, int, ZipExtraField[])} with {@code Resource}, {@code ZipOutputStream}, {@code String}, {@code int}, {@code ZipExtraField[]}.
   * <p>
   * Method under test: {@link Zip#zipDir(Resource, ZipOutputStream, String, int, ZipExtraField[])}
   */
  @Test
  public void testZipDirWithResourceZipOutputStreamStringIntZipExtraField3() throws IOException {
    // Arrange
    Zip zip = new Zip();
    Resource dir = new Resource();

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.putNextEntry(new ZipEntry("adding directory "));

    // Act
    zip.zipDir(dir, zOut, "V Path", 1, new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(Resource, ZipOutputStream, String, int, ZipExtraField[])} with {@code Resource}, {@code ZipOutputStream}, {@code String}, {@code int}, {@code ZipExtraField[]}.
   * <p>
   * Method under test: {@link Zip#zipDir(Resource, ZipOutputStream, String, int, ZipExtraField[])}
   */
  @Test
  public void testZipDirWithResourceZipOutputStreamStringIntZipExtraField4() throws IOException {
    // Arrange
    Zip zip = new Zip();
    Resource dir = new Resource();
    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));

    // Act
    zip.zipDir(dir, zOut, "/", 1, new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("/", stringStringMap.get("/"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(Resource, ZipOutputStream, String, int, ZipExtraField[])} with {@code Resource}, {@code ZipOutputStream}, {@code String}, {@code int}, {@code ZipExtraField[]}.
   * <p>
   * Method under test: {@link Zip#zipDir(Resource, ZipOutputStream, String, int, ZipExtraField[])}
   */
  @Test
  public void testZipDirWithResourceZipOutputStreamStringIntZipExtraField5() throws IOException {
    // Arrange
    Zip zip = new Zip();
    Resource dir = new Resource();

    // Act
    zip.zipDir(dir, new ZipOutputStream(new ByteArrayOutputStream(1)), "V Path", 1, null);

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(Resource, ZipOutputStream, String, int, ZipExtraField[])} with {@code Resource}, {@code ZipOutputStream}, {@code String}, {@code int}, {@code ZipExtraField[]}.
   * <p>
   * Method under test: {@link Zip#zipDir(Resource, ZipOutputStream, String, int, ZipExtraField[])}
   */
  @Test
  public void testZipDirWithResourceZipOutputStreamStringIntZipExtraField6() throws IOException {
    // Arrange
    Zip zip = new Zip();
    zip.setFilesonly(true);
    Resource dir = new Resource();

    // Act
    zip.zipDir(dir, null, "V Path", 1, new ZipExtraField[]{new AsiExtraField()});

    // Assert that nothing has changed
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
    assertTrue(zip.addedDirs.isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(Resource, ZipOutputStream, String, int, ZipExtraField[])} with {@code Resource}, {@code ZipOutputStream}, {@code String}, {@code int}, {@code ZipExtraField[]}.
   * <ul>
   *   <li>Given {@code Always}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipDir(Resource, ZipOutputStream, String, int, ZipExtraField[])}
   */
  @Test
  public void testZipDirWithResourceZipOutputStreamStringIntZipExtraField_givenAlways() throws IOException {
    // Arrange
    Zip zip = new Zip();
    Resource dir = new Resource();

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.setUseZip64(Zip64Mode.Always);
    zOut.putNextEntry(new ZipEntry("adding directory "));

    // Act
    zip.zipDir(dir, zOut, "V Path", 1, new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(Resource, ZipOutputStream, String, int, ZipExtraField[])} with {@code Resource}, {@code ZipOutputStream}, {@code String}, {@code int}, {@code ZipExtraField[]}.
   * <ul>
   *   <li>Given {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipDir(Resource, ZipOutputStream, String, int, ZipExtraField[])}
   */
  @Test
  public void testZipDirWithResourceZipOutputStreamStringIntZipExtraField_givenNull() throws IOException {
    // Arrange
    Zip zip = new Zip();
    Resource dir = new Resource();

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.setCreateUnicodeExtraFields(null);
    zOut.putNextEntry(new ZipEntry("adding directory "));

    // Act
    zip.zipDir(dir, zOut, "V Path", 1, new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#zipDir(Resource, ZipOutputStream, String, int, ZipExtraField[])} with {@code Resource}, {@code ZipOutputStream}, {@code String}, {@code int}, {@code ZipExtraField[]}.
   * <ul>
   *   <li>When minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipDir(Resource, ZipOutputStream, String, int, ZipExtraField[])}
   */
  @Test
  public void testZipDirWithResourceZipOutputStreamStringIntZipExtraField_whenMinusOne() throws IOException {
    // Arrange
    Zip zip = new Zip();
    Resource dir = new Resource();
    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));

    // Act
    zip.zipDir(dir, zOut, "V Path", -1, new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertTrue(zip.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Zip#getCurrentExtraFields()}.
   * <p>
   * Method under test: {@link Zip#getCurrentExtraFields()}
   */
  @Test
  public void testGetCurrentExtraFields() {
    // Arrange, Act and Assert
    assertNull((new Zip()).getCurrentExtraFields());
  }

  /**
   * Test {@link Zip#zipFile(File, ZipOutputStream, String, int)} with {@code file}, {@code zOut}, {@code vPath}, {@code mode}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipFile(File, ZipOutputStream, String, int)}
   */
  @Test
  public void testZipFileWithFileZOutVPathMode_thenThrowBuildException() throws IOException {
    // Arrange
    Zip zip = new Zip();
    zip.setDestFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> zip.zipFile(Copy.NULL_FILE_PLACEHOLDER, null, "V Path", 1));
  }

  /**
   * Test {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int)} with {@code in}, {@code zOut}, {@code vPath}, {@code lastModified}, {@code fromArchive}, {@code mode}.
   * <p>
   * Method under test: {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int)}
   */
  @Test
  public void testZipFileWithInZOutVPathLastModifiedFromArchiveMode() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ByteArrayInputStream in = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    zip.zipFile(in, new ZipOutputStream(new ByteArrayOutputStream(1)), "V Path", WaitFor.ONE_MILLISECOND,
        Copy.NULL_FILE_PLACEHOLDER, 1);

    // Assert
    Hashtable<String, String> stringStringMap = zip.entries;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertEquals(-1, in.read(new byte[]{}));
  }

  /**
   * Test {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int)} with {@code in}, {@code zOut}, {@code vPath}, {@code lastModified}, {@code fromArchive}, {@code mode}.
   * <p>
   * Method under test: {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int)}
   */
  @Test
  public void testZipFileWithInZOutVPathLastModifiedFromArchiveMode2() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ByteArrayInputStream in = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.putNextEntry(new ZipEntry("adding entry "));

    // Act
    zip.zipFile(in, zOut, "V Path", WaitFor.ONE_MILLISECOND, Copy.NULL_FILE_PLACEHOLDER, 1);

    // Assert
    Hashtable<String, String> stringStringMap = zip.entries;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertEquals(-1, in.read(new byte[]{}));
  }

  /**
   * Test {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int, ZipExtraField[])} with {@code in}, {@code zOut}, {@code vPath}, {@code lastModified}, {@code fromArchive}, {@code mode}, {@code extra}.
   * <p>
   * Method under test: {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int, ZipExtraField[])}
   */
  @Test
  public void testZipFileWithInZOutVPathLastModifiedFromArchiveModeExtra() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ByteArrayInputStream in = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));
    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));

    // Act
    zip.zipFile(in, zOut, "V Path", WaitFor.ONE_MILLISECOND, Copy.NULL_FILE_PLACEHOLDER, 1,
        new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.entries;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertEquals(-1, in.read(new byte[]{}));
  }

  /**
   * Test {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int, ZipExtraField[])} with {@code in}, {@code zOut}, {@code vPath}, {@code lastModified}, {@code fromArchive}, {@code mode}, {@code extra}.
   * <p>
   * Method under test: {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int, ZipExtraField[])}
   */
  @Test
  public void testZipFileWithInZOutVPathLastModifiedFromArchiveModeExtra2() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ByteArrayInputStream in = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.putNextEntry(new ZipEntry("adding entry "));

    // Act
    zip.zipFile(in, zOut, "V Path", WaitFor.ONE_MILLISECOND, Copy.NULL_FILE_PLACEHOLDER, 1,
        new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.entries;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertEquals(-1, in.read(new byte[]{}));
  }

  /**
   * Test {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int, ZipExtraField[])} with {@code in}, {@code zOut}, {@code vPath}, {@code lastModified}, {@code fromArchive}, {@code mode}, {@code extra}.
   * <p>
   * Method under test: {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int, ZipExtraField[])}
   */
  @Test
  public void testZipFileWithInZOutVPathLastModifiedFromArchiveModeExtra3() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ByteArrayInputStream in = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));
    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));

    // Act
    zip.zipFile(in, zOut, "/", WaitFor.ONE_MILLISECOND, Copy.NULL_FILE_PLACEHOLDER, 1,
        new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.entries;
    assertEquals(1, stringStringMap.size());
    assertEquals("/", stringStringMap.get("/"));
    assertEquals(-1, in.read(new byte[]{}));
  }

  /**
   * Test {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int, ZipExtraField[])} with {@code in}, {@code zOut}, {@code vPath}, {@code lastModified}, {@code fromArchive}, {@code mode}, {@code extra}.
   * <p>
   * Method under test: {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int, ZipExtraField[])}
   */
  @Test
  public void testZipFileWithInZOutVPathLastModifiedFromArchiveModeExtra4() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ByteArrayInputStream in = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));
    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));

    // Act
    zip.zipFile(in, zOut, "V Path", WaitFor.ONE_MILLISECOND, Copy.NULL_FILE_PLACEHOLDER, 1,
        new ZipExtraField[]{JarMarker.getInstance()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.entries;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertEquals(-1, in.read(new byte[]{}));
  }

  /**
   * Test {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int, ZipExtraField[])} with {@code in}, {@code zOut}, {@code vPath}, {@code lastModified}, {@code fromArchive}, {@code mode}, {@code extra}.
   * <p>
   * Method under test: {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int, ZipExtraField[])}
   */
  @Test
  public void testZipFileWithInZOutVPathLastModifiedFromArchiveModeExtra5() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ByteArrayInputStream in = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    zip.zipFile(in, new ZipOutputStream(new ByteArrayOutputStream(1)), "V Path", WaitFor.ONE_MILLISECOND,
        Copy.NULL_FILE_PLACEHOLDER, 1, new ZipExtraField[]{});

    // Assert
    Hashtable<String, String> stringStringMap = zip.entries;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertEquals(-1, in.read(new byte[]{}));
  }

  /**
   * Test {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int, ZipExtraField[])} with {@code in}, {@code zOut}, {@code vPath}, {@code lastModified}, {@code fromArchive}, {@code mode}, {@code extra}.
   * <ul>
   *   <li>Given {@code Always}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int, ZipExtraField[])}
   */
  @Test
  public void testZipFileWithInZOutVPathLastModifiedFromArchiveModeExtra_givenAlways() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ByteArrayInputStream in = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.setUseZip64(Zip64Mode.Always);
    zOut.putNextEntry(new ZipEntry("adding entry "));

    // Act
    zip.zipFile(in, zOut, "V Path", WaitFor.ONE_MILLISECOND, Copy.NULL_FILE_PLACEHOLDER, 1,
        new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.entries;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertEquals(-1, in.read(new byte[]{}));
  }

  /**
   * Test {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int, ZipExtraField[])} with {@code in}, {@code zOut}, {@code vPath}, {@code lastModified}, {@code fromArchive}, {@code mode}, {@code extra}.
   * <ul>
   *   <li>Given {@code Always}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int, ZipExtraField[])}
   */
  @Test
  public void testZipFileWithInZOutVPathLastModifiedFromArchiveModeExtra_givenAlways2() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ByteArrayInputStream in = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.setUseZip64(Zip64Mode.Always);

    // Act
    zip.zipFile(in, zOut, "V Path", WaitFor.ONE_MILLISECOND, Copy.NULL_FILE_PLACEHOLDER, 1,
        new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.entries;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertEquals(-1, in.read(new byte[]{}));
  }

  /**
   * Test {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int, ZipExtraField[])} with {@code in}, {@code zOut}, {@code vPath}, {@code lastModified}, {@code fromArchive}, {@code mode}, {@code extra}.
   * <ul>
   *   <li>Given {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int, ZipExtraField[])}
   */
  @Test
  public void testZipFileWithInZOutVPathLastModifiedFromArchiveModeExtra_givenNull() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ByteArrayInputStream in = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.setCreateUnicodeExtraFields(null);
    zOut.putNextEntry(new ZipEntry("adding entry "));

    // Act
    zip.zipFile(in, zOut, "V Path", WaitFor.ONE_MILLISECOND, Copy.NULL_FILE_PLACEHOLDER, 1,
        new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.entries;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertEquals(-1, in.read(new byte[]{}));
  }

  /**
   * Test {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int, ZipExtraField[])} with {@code in}, {@code zOut}, {@code vPath}, {@code lastModified}, {@code fromArchive}, {@code mode}, {@code extra}.
   * <ul>
   *   <li>When minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int, ZipExtraField[])}
   */
  @Test
  public void testZipFileWithInZOutVPathLastModifiedFromArchiveModeExtra_whenMinusOne() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ByteArrayInputStream in = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));
    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));

    // Act
    zip.zipFile(in, zOut, "V Path", -1L, Copy.NULL_FILE_PLACEHOLDER, 1, new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.entries;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertEquals(-1, in.read(new byte[]{}));
  }

  /**
   * Test {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int, ZipExtraField[])} with {@code in}, {@code zOut}, {@code vPath}, {@code lastModified}, {@code fromArchive}, {@code mode}, {@code extra}.
   * <ul>
   *   <li>When minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int, ZipExtraField[])}
   */
  @Test
  public void testZipFileWithInZOutVPathLastModifiedFromArchiveModeExtra_whenMinusOne2() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ByteArrayInputStream in = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));
    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));

    // Act
    zip.zipFile(in, zOut, "V Path", WaitFor.ONE_MILLISECOND, Copy.NULL_FILE_PLACEHOLDER, -1,
        new ZipExtraField[]{new AsiExtraField()});

    // Assert
    Hashtable<String, String> stringStringMap = zip.entries;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertEquals(-1, in.read(new byte[]{}));
  }

  /**
   * Test {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int, ZipExtraField[])} with {@code in}, {@code zOut}, {@code vPath}, {@code lastModified}, {@code fromArchive}, {@code mode}, {@code extra}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int, ZipExtraField[])}
   */
  @Test
  public void testZipFileWithInZOutVPathLastModifiedFromArchiveModeExtra_whenNull() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ByteArrayInputStream in = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    zip.zipFile(in, new ZipOutputStream(new ByteArrayOutputStream(1)), "V Path", WaitFor.ONE_MILLISECOND,
        Copy.NULL_FILE_PLACEHOLDER, 1, null);

    // Assert
    Hashtable<String, String> stringStringMap = zip.entries;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertEquals(-1, in.read(new byte[]{}));
  }

  /**
   * Test {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int)} with {@code in}, {@code zOut}, {@code vPath}, {@code lastModified}, {@code fromArchive}, {@code mode}.
   * <ul>
   *   <li>Given {@code Always}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int)}
   */
  @Test
  public void testZipFileWithInZOutVPathLastModifiedFromArchiveMode_givenAlways() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ByteArrayInputStream in = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.setUseZip64(Zip64Mode.Always);
    zOut.putNextEntry(new ZipEntry("adding entry "));

    // Act
    zip.zipFile(in, zOut, "V Path", WaitFor.ONE_MILLISECOND, Copy.NULL_FILE_PLACEHOLDER, 1);

    // Assert
    Hashtable<String, String> stringStringMap = zip.entries;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertEquals(-1, in.read(new byte[]{}));
  }

  /**
   * Test {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int)} with {@code in}, {@code zOut}, {@code vPath}, {@code lastModified}, {@code fromArchive}, {@code mode}.
   * <ul>
   *   <li>Given {@code Always}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int)}
   */
  @Test
  public void testZipFileWithInZOutVPathLastModifiedFromArchiveMode_givenAlways2() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ByteArrayInputStream in = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.setUseZip64(Zip64Mode.Always);

    // Act
    zip.zipFile(in, zOut, "V Path", WaitFor.ONE_MILLISECOND, Copy.NULL_FILE_PLACEHOLDER, 1);

    // Assert
    Hashtable<String, String> stringStringMap = zip.entries;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertEquals(-1, in.read(new byte[]{}));
  }

  /**
   * Test {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int)} with {@code in}, {@code zOut}, {@code vPath}, {@code lastModified}, {@code fromArchive}, {@code mode}.
   * <ul>
   *   <li>Given {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int)}
   */
  @Test
  public void testZipFileWithInZOutVPathLastModifiedFromArchiveMode_givenNull() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ByteArrayInputStream in = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.setCreateUnicodeExtraFields(null);
    zOut.putNextEntry(new ZipEntry("adding entry "));

    // Act
    zip.zipFile(in, zOut, "V Path", WaitFor.ONE_MILLISECOND, Copy.NULL_FILE_PLACEHOLDER, 1);

    // Assert
    Hashtable<String, String> stringStringMap = zip.entries;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertEquals(-1, in.read(new byte[]{}));
  }

  /**
   * Test {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int)} with {@code in}, {@code zOut}, {@code vPath}, {@code lastModified}, {@code fromArchive}, {@code mode}.
   * <ul>
   *   <li>Then {@link Zip} (default constructor) {@link Zip#entries} {@code /} is {@code /}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int)}
   */
  @Test
  public void testZipFileWithInZOutVPathLastModifiedFromArchiveMode_thenZipEntriesSlashIsSlash() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ByteArrayInputStream in = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    zip.zipFile(in, new ZipOutputStream(new ByteArrayOutputStream(1)), "/", WaitFor.ONE_MILLISECOND,
        Copy.NULL_FILE_PLACEHOLDER, 1);

    // Assert
    Hashtable<String, String> stringStringMap = zip.entries;
    assertEquals(1, stringStringMap.size());
    assertEquals("/", stringStringMap.get("/"));
    assertEquals(-1, in.read(new byte[]{}));
  }

  /**
   * Test {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int)} with {@code in}, {@code zOut}, {@code vPath}, {@code lastModified}, {@code fromArchive}, {@code mode}.
   * <ul>
   *   <li>When minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int)}
   */
  @Test
  public void testZipFileWithInZOutVPathLastModifiedFromArchiveMode_whenMinusOne() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ByteArrayInputStream in = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    zip.zipFile(in, new ZipOutputStream(new ByteArrayOutputStream(1)), "V Path", -1L, Copy.NULL_FILE_PLACEHOLDER, 1);

    // Assert
    Hashtable<String, String> stringStringMap = zip.entries;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertEquals(-1, in.read(new byte[]{}));
  }

  /**
   * Test {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int)} with {@code in}, {@code zOut}, {@code vPath}, {@code lastModified}, {@code fromArchive}, {@code mode}.
   * <ul>
   *   <li>When minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#zipFile(InputStream, ZipOutputStream, String, long, File, int)}
   */
  @Test
  public void testZipFileWithInZOutVPathLastModifiedFromArchiveMode_whenMinusOne2() throws IOException {
    // Arrange
    Zip zip = new Zip();
    ByteArrayInputStream in = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    zip.zipFile(in, new ZipOutputStream(new ByteArrayOutputStream(1)), "V Path", WaitFor.ONE_MILLISECOND,
        Copy.NULL_FILE_PLACEHOLDER, -1);

    // Assert
    Hashtable<String, String> stringStringMap = zip.entries;
    assertEquals(1, stringStringMap.size());
    assertEquals("V Path", stringStringMap.get("V Path"));
    assertEquals(-1, in.read(new byte[]{}));
  }

  /**
   * Test {@link Zip#isEmpty(Resource[][])}.
   * <ul>
   *   <li>When array of {@link Resource} with {@link Resource#Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#isEmpty(Resource[][])}
   */
  @Test
  public void testIsEmpty_whenArrayOfResourceWithResource_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(Zip.isEmpty(new Resource[][]{new Resource[]{new Resource()}}));
  }

  /**
   * Test {@link Zip#isEmpty(Resource[][])}.
   * <ul>
   *   <li>When empty array of {@link Resource}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#isEmpty(Resource[][])}
   */
  @Test
  public void testIsEmpty_whenEmptyArrayOfResource_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(Zip.isEmpty(new Resource[][]{new Resource[]{}}));
  }

  /**
   * Test {@link Zip#selectFileResources(Resource[])}.
   * <p>
   * Method under test: {@link Zip#selectFileResources(Resource[])}
   */
  @Test
  public void testSelectFileResources() {
    // Arrange
    Zip zip = new Zip();
    Resource[] orig = new Resource[]{new Resource(Manifest.ATTRIBUTE_NAME)};

    // Act and Assert
    assertSame(orig, zip.selectFileResources(orig));
  }

  /**
   * Test {@link Zip#selectFileResources(Resource[])}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@link Resource#Resource()} Directory is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectFileResources(Resource[])}
   */
  @Test
  public void testSelectFileResources_givenJavaLangObject_whenResourceDirectoryIsTrue() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(" as only files will be added.", typeClass);
    project.addBuildListener(new AntClassLoader());

    Zip zip = new Zip();
    zip.setProject(project);
    zip.setFilesonly(true);

    Resource resource = new Resource();
    resource.setDirectory(true);

    // Act and Assert
    assertEquals(0, zip.selectFileResources(new Resource[]{resource}).length);
  }

  /**
   * Test {@link Zip#selectFileResources(Resource[])}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectFileResources(Resource[])}
   */
  @Test
  public void testSelectFileResources_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Zip zip = new Zip();
    zip.setProject(project);
    zip.setFilesonly(true);

    Resource resource = new Resource();
    resource.setDirectory(true);

    // Act and Assert
    assertEquals(0, zip.selectFileResources(new Resource[]{resource}).length);
  }

  /**
   * Test {@link Zip#selectFileResources(Resource[])}.
   * <ul>
   *   <li>Given {@code true}.</li>
   *   <li>When {@link Resource#Resource()} Directory is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectFileResources(Resource[])}
   */
  @Test
  public void testSelectFileResources_givenTrue_whenResourceDirectoryIsTrue() {
    // Arrange
    Zip zip = new Zip();

    Resource resource = new Resource();
    resource.setDirectory(true);

    // Act and Assert
    assertEquals(0, zip.selectFileResources(new Resource[]{resource}).length);
  }

  /**
   * Test {@link Zip#selectFileResources(Resource[])}.
   * <ul>
   *   <li>Given {@link Zip} (default constructor) Filesonly is {@code true}.</li>
   *   <li>When {@link Resource#Resource()} Directory is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectFileResources(Resource[])}
   */
  @Test
  public void testSelectFileResources_givenZipFilesonlyIsTrue_whenResourceDirectoryIsTrue() {
    // Arrange
    Zip zip = new Zip();
    zip.setFilesonly(true);

    Resource resource = new Resource();
    resource.setDirectory(true);

    // Act and Assert
    assertEquals(0, zip.selectFileResources(new Resource[]{resource}).length);
  }

  /**
   * Test {@link Zip#selectFileResources(Resource[])}.
   * <ul>
   *   <li>Given {@link Zip} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>When {@link Resource#Resource()} Directory is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectFileResources(Resource[])}
   */
  @Test
  public void testSelectFileResources_givenZipProjectIsProject_whenResourceDirectoryIsTrue() {
    // Arrange
    Zip zip = new Zip();
    zip.setProject(new Project());
    zip.setFilesonly(true);

    Resource resource = new Resource();
    resource.setDirectory(true);

    // Act and Assert
    assertEquals(0, zip.selectFileResources(new Resource[]{resource}).length);
  }

  /**
   * Test {@link Zip#selectFileResources(Resource[])}.
   * <ul>
   *   <li>Given {@link Zip} (default constructor).</li>
   *   <li>Then return array of {@link Resource} with {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectFileResources(Resource[])}
   */
  @Test
  public void testSelectFileResources_givenZip_thenReturnArrayOfResourceWithResource() {
    // Arrange
    Zip zip = new Zip();
    Resource[] orig = new Resource[]{new Resource()};

    // Act and Assert
    assertSame(orig, zip.selectFileResources(orig));
  }

  /**
   * Test {@link Zip#selectFileResources(Resource[])}.
   * <ul>
   *   <li>Then return array of {@link Resource} with {@link Resource#Resource()} and {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectFileResources(Resource[])}
   */
  @Test
  public void testSelectFileResources_thenReturnArrayOfResourceWithResourceAndResource() {
    // Arrange
    Zip zip = new Zip();
    Resource resource = new Resource();
    Resource[] orig = new Resource[]{resource, new Resource()};

    // Act and Assert
    assertSame(orig, zip.selectFileResources(orig));
  }

  /**
   * Test {@link Zip#selectFileResources(Resource[])}.
   * <ul>
   *   <li>When empty array of {@link Resource}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectFileResources(Resource[])}
   */
  @Test
  public void testSelectFileResources_whenEmptyArrayOfResource_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new Zip()).selectFileResources(new Resource[]{}).length);
  }

  /**
   * Test {@link Zip#selectDirectoryResources(Resource[])}.
   * <p>
   * Method under test: {@link Zip#selectDirectoryResources(Resource[])}
   */
  @Test
  public void testSelectDirectoryResources() {
    // Arrange
    Zip zip = new Zip();

    // Act and Assert
    assertEquals(0, zip.selectDirectoryResources(new Resource[]{new Resource(Manifest.ATTRIBUTE_NAME)}).length);
  }

  /**
   * Test {@link Zip#selectDirectoryResources(Resource[])}.
   * <ul>
   *   <li>Given {@code true}.</li>
   *   <li>Then return array of {@link Resource} with {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectDirectoryResources(Resource[])}
   */
  @Test
  public void testSelectDirectoryResources_givenTrue_thenReturnArrayOfResourceWithResource() {
    // Arrange
    Zip zip = new Zip();

    Resource resource = new Resource();
    resource.setDirectory(true);
    Resource[] orig = new Resource[]{resource};

    // Act and Assert
    assertSame(orig, zip.selectDirectoryResources(orig));
  }

  /**
   * Test {@link Zip#selectDirectoryResources(Resource[])}.
   * <ul>
   *   <li>When array of {@link Resource} with {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectDirectoryResources(Resource[])}
   */
  @Test
  public void testSelectDirectoryResources_whenArrayOfResourceWithResource() {
    // Arrange
    Zip zip = new Zip();

    // Act and Assert
    assertEquals(0, zip.selectDirectoryResources(new Resource[]{new Resource()}).length);
  }

  /**
   * Test {@link Zip#selectDirectoryResources(Resource[])}.
   * <ul>
   *   <li>When array of {@link Resource} with {@link Resource#Resource()} and {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectDirectoryResources(Resource[])}
   */
  @Test
  public void testSelectDirectoryResources_whenArrayOfResourceWithResourceAndResource() {
    // Arrange
    Zip zip = new Zip();
    Resource resource = new Resource();

    // Act and Assert
    assertEquals(0, zip.selectDirectoryResources(new Resource[]{resource, new Resource()}).length);
  }

  /**
   * Test {@link Zip#selectDirectoryResources(Resource[])}.
   * <ul>
   *   <li>When empty array of {@link Resource}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectDirectoryResources(Resource[])}
   */
  @Test
  public void testSelectDirectoryResources_whenEmptyArrayOfResource() {
    // Arrange, Act and Assert
    assertEquals(0, (new Zip()).selectDirectoryResources(new Resource[]{}).length);
  }

  /**
   * Test {@link Zip#selectResources(Resource[], ResourceSelector)}.
   * <p>
   * Method under test: {@link Zip#selectResources(Resource[], ResourceSelector)}
   */
  @Test
  public void testSelectResources() {
    // Arrange
    Zip zip = new Zip();
    Resource[] orig = new Resource[]{new Resource(Manifest.ATTRIBUTE_NAME, true, WaitFor.ONE_MILLISECOND)};

    // Act and Assert
    assertSame(orig, zip.selectResources(orig, new Exists()));
  }

  /**
   * Test {@link Zip#selectResources(Resource[], ResourceSelector)}.
   * <ul>
   *   <li>Given {@code true}.</li>
   *   <li>When {@link Resource#Resource()} Directory is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectResources(Resource[], ResourceSelector)}
   */
  @Test
  public void testSelectResources_givenTrue_whenResourceDirectoryIsTrue() {
    // Arrange
    Zip zip = new Zip();

    Resource resource = new Resource();
    resource.setDirectory(true);
    Resource[] orig = new Resource[]{resource};

    // Act and Assert
    assertSame(orig, zip.selectResources(orig, Type.DIR));
  }

  /**
   * Test {@link Zip#selectResources(Resource[], ResourceSelector)}.
   * <ul>
   *   <li>Then return array of {@link Resource} with {@link Resource#Resource()} and {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectResources(Resource[], ResourceSelector)}
   */
  @Test
  public void testSelectResources_thenReturnArrayOfResourceWithResourceAndResource() {
    // Arrange
    Zip zip = new Zip();
    Resource resource = new Resource();
    Resource[] orig = new Resource[]{resource, new Resource()};

    // Act and Assert
    assertSame(orig, zip.selectResources(orig, new Exists()));
  }

  /**
   * Test {@link Zip#selectResources(Resource[], ResourceSelector)}.
   * <ul>
   *   <li>When {@link And#And(ResourceSelector[])} with r is {@link Type#ANY}.</li>
   *   <li>Then return array of {@link Resource} with {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectResources(Resource[], ResourceSelector)}
   */
  @Test
  public void testSelectResources_whenAndWithRIsAny_thenReturnArrayOfResourceWithResource() {
    // Arrange
    Zip zip = new Zip();
    Resource[] orig = new Resource[]{new Resource()};

    // Act and Assert
    assertSame(orig, zip.selectResources(orig, new And(Type.ANY)));
  }

  /**
   * Test {@link Zip#selectResources(Resource[], ResourceSelector)}.
   * <ul>
   *   <li>When {@link And#And()}.</li>
   *   <li>Then return array of {@link Resource} with {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectResources(Resource[], ResourceSelector)}
   */
  @Test
  public void testSelectResources_whenAnd_thenReturnArrayOfResourceWithResource() {
    // Arrange
    Zip zip = new Zip();
    Resource[] orig = new Resource[]{new Resource()};

    // Act and Assert
    assertSame(orig, zip.selectResources(orig, new And()));
  }

  /**
   * Test {@link Zip#selectResources(Resource[], ResourceSelector)}.
   * <ul>
   *   <li>When {@link And#And()}.</li>
   *   <li>Then return array of {@link Resource} with {@link Resource#Resource()} and {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectResources(Resource[], ResourceSelector)}
   */
  @Test
  public void testSelectResources_whenAnd_thenReturnArrayOfResourceWithResourceAndResource() {
    // Arrange
    Zip zip = new Zip();
    Resource resource = new Resource();
    Resource[] orig = new Resource[]{resource, new Resource()};

    // Act and Assert
    assertSame(orig, zip.selectResources(orig, new And()));
  }

  /**
   * Test {@link Zip#selectResources(Resource[], ResourceSelector)}.
   * <ul>
   *   <li>When {@link Type#ANY}.</li>
   *   <li>Then return array of {@link Resource} with {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectResources(Resource[], ResourceSelector)}
   */
  @Test
  public void testSelectResources_whenAny_thenReturnArrayOfResourceWithResource() {
    // Arrange
    Zip zip = new Zip();
    Resource[] orig = new Resource[]{new Resource()};

    // Act and Assert
    assertSame(orig, zip.selectResources(orig, Type.ANY));
  }

  /**
   * Test {@link Zip#selectResources(Resource[], ResourceSelector)}.
   * <ul>
   *   <li>When array of {@link Resource} with {@link Resource#Resource()} and {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectResources(Resource[], ResourceSelector)}
   */
  @Test
  public void testSelectResources_whenArrayOfResourceWithResourceAndResource() {
    // Arrange
    Zip zip = new Zip();
    Resource resource = new Resource();

    // Act and Assert
    assertEquals(0, zip.selectResources(new Resource[]{resource, new Resource()}, Type.DIR).length);
  }

  /**
   * Test {@link Zip#selectResources(Resource[], ResourceSelector)}.
   * <ul>
   *   <li>When array of {@link Resource} with {@link Resource#Resource(String)} with name is {@link Manifest#ATTRIBUTE_NAME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectResources(Resource[], ResourceSelector)}
   */
  @Test
  public void testSelectResources_whenArrayOfResourceWithResourceWithNameIsAttribute_name() {
    // Arrange
    Zip zip = new Zip();

    // Act and Assert
    assertEquals(0, zip.selectResources(new Resource[]{new Resource(Manifest.ATTRIBUTE_NAME)}, Type.DIR).length);
  }

  /**
   * Test {@link Zip#selectResources(Resource[], ResourceSelector)}.
   * <ul>
   *   <li>When array of {@link Resource} with {@link Resource#Resource(String)} with name is {@link Manifest#ATTRIBUTE_NAME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectResources(Resource[], ResourceSelector)}
   */
  @Test
  public void testSelectResources_whenArrayOfResourceWithResourceWithNameIsAttribute_name2() {
    // Arrange
    Zip zip = new Zip();

    // Act and Assert
    assertEquals(0, zip.selectResources(new Resource[]{new Resource(Manifest.ATTRIBUTE_NAME)}, new Exists()).length);
  }

  /**
   * Test {@link Zip#selectResources(Resource[], ResourceSelector)}.
   * <ul>
   *   <li>When {@link Type#DIR}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectResources(Resource[], ResourceSelector)}
   */
  @Test
  public void testSelectResources_whenDir_thenReturnArrayLengthIsZero() {
    // Arrange
    Zip zip = new Zip();

    // Act and Assert
    assertEquals(0, zip.selectResources(new Resource[]{new Resource()}, Type.DIR).length);
  }

  /**
   * Test {@link Zip#selectResources(Resource[], ResourceSelector)}.
   * <ul>
   *   <li>When empty array of {@link Resource}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectResources(Resource[], ResourceSelector)}
   */
  @Test
  public void testSelectResources_whenEmptyArrayOfResource_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new Zip()).selectResources(new Resource[]{}, Type.ANY).length);
  }

  /**
   * Test {@link Zip#selectResources(Resource[], ResourceSelector)}.
   * <ul>
   *   <li>When {@link Exists} (default constructor).</li>
   *   <li>Then return array of {@link Resource} with {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectResources(Resource[], ResourceSelector)}
   */
  @Test
  public void testSelectResources_whenExists_thenReturnArrayOfResourceWithResource() {
    // Arrange
    Zip zip = new Zip();
    Resource[] orig = new Resource[]{new Resource()};

    // Act and Assert
    assertSame(orig, zip.selectResources(orig, new Exists()));
  }

  /**
   * Test {@link Zip#selectResources(Resource[], ResourceSelector)}.
   * <ul>
   *   <li>When {@link ScriptSelector} (default constructor).</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectResources(Resource[], ResourceSelector)}
   */
  @Test
  public void testSelectResources_whenScriptSelector_thenReturnArrayLengthIsZero() {
    // Arrange
    Zip zip = new Zip();

    // Act and Assert
    assertEquals(0, zip.selectResources(new Resource[]{new Resource()}, new ScriptSelector()).length);
  }

  /**
   * Test {@link Zip#selectResources(Resource[], ResourceSelector)}.
   * <ul>
   *   <li>When {@link ScriptSelector} (default constructor).</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip#selectResources(Resource[], ResourceSelector)}
   */
  @Test
  public void testSelectResources_whenScriptSelector_thenReturnArrayLengthIsZero2() {
    // Arrange
    Zip zip = new Zip();
    Resource resource = new Resource();

    // Act and Assert
    assertEquals(0, zip.selectResources(new Resource[]{resource, new Resource()}, new ScriptSelector()).length);
  }

  /**
   * Test new {@link Zip} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Zip}
   */
  @Test
  public void testNewZip() {
    // Arrange and Act
    Zip actualZip = new Zip();

    // Assert
    assertEquals("", actualZip.getComment());
    assertEquals("add", actualZip.duplicate);
    assertEquals("skip", actualZip.emptyBehavior);
    assertEquals("zip", actualZip.archiveType);
    assertNull(actualZip.getCurrentExtraFields());
    assertNull(actualZip.getDestFile());
    assertNull(actualZip.getDescription());
    assertNull(actualZip.getTaskName());
    assertNull(actualZip.getTaskType());
    assertNull(actualZip.getEncoding());
    assertNull(actualZip.getModificationtime());
    assertNull(actualZip.getProject());
    assertNull(actualZip.getOwningTarget());
    assertEquals(-1, actualZip.getLevel());
    assertFalse(actualZip.hasSelectors());
    assertFalse(actualZip.getFallBackToUTF8());
    assertFalse(actualZip.getPreserve0Permissions());
    assertFalse(actualZip.hasUpdatedFile());
    assertFalse(actualZip.isAddingNewFiles());
    assertFalse(actualZip.isInUpdateMode());
    assertFalse(actualZip.doubleFilePass);
    assertFalse(actualZip.skipWriting);
    assertTrue(actualZip.addedDirs.isEmpty());
    assertTrue(actualZip.entries.isEmpty());
    assertTrue(actualZip.getUseLanguageEnodingFlag());
    assertTrue(actualZip.isCompress());
    assertTrue(actualZip.isFirstPass());
  }
}
