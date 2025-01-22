package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.Hashtable;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.Definer.OnError;
import org.apache.tools.ant.taskdefs.Jar.FilesetManifestConfig;
import org.apache.tools.ant.taskdefs.Jar.StrictMode;
import org.apache.tools.ant.taskdefs.Zip.ArchiveState;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.ZipFileSet;
import org.apache.tools.ant.types.resources.Resources;
import org.apache.tools.zip.AsiExtraField;
import org.apache.tools.zip.Zip64Mode;
import org.apache.tools.zip.ZipEntry;
import org.apache.tools.zip.ZipOutputStream;
import org.junit.Test;

public class JarDiffblueTest {
  /**
   * Test FilesetManifestConfig {@link FilesetManifestConfig#getValues()}.
   * <p>
   * Method under test: {@link FilesetManifestConfig#getValues()}
   */
  @Test
  public void testFilesetManifestConfigGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"skip", "merge", "mergewithoutmain"}, (new FilesetManifestConfig()).getValues());
  }

  /**
   * Test FilesetManifestConfig new {@link FilesetManifestConfig} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link FilesetManifestConfig}
   */
  @Test
  public void testFilesetManifestConfigNewFilesetManifestConfig() {
    // Arrange and Act
    FilesetManifestConfig actualFilesetManifestConfig = new FilesetManifestConfig();

    // Assert
    assertNull(actualFilesetManifestConfig.getValue());
    assertEquals(-1, actualFilesetManifestConfig.getIndex());
  }

  /**
   * Test new {@link Jar} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Jar}
   */
  @Test
  public void testNewJar() {
    // Arrange and Act
    Jar actualJar = new Jar();

    // Assert
    assertEquals("", actualJar.getComment());
    assertEquals("UTF8", actualJar.getEncoding());
    assertEquals("add", actualJar.duplicate);
    assertEquals("create", actualJar.emptyBehavior);
    assertEquals("jar", actualJar.archiveType);
    assertNull(actualJar.getCurrentExtraFields());
    assertNull(actualJar.getDestFile());
    assertNull(actualJar.getDescription());
    assertNull(actualJar.getTaskName());
    assertNull(actualJar.getTaskType());
    assertNull(actualJar.getModificationtime());
    assertNull(actualJar.getProject());
    assertNull(actualJar.getOwningTarget());
    assertNull(actualJar.getIndexJarsMapper());
    assertEquals(-1, actualJar.getLevel());
    assertFalse(actualJar.hasSelectors());
    assertFalse(actualJar.getFallBackToUTF8());
    assertFalse(actualJar.getPreserve0Permissions());
    assertFalse(actualJar.hasUpdatedFile());
    assertFalse(actualJar.isAddingNewFiles());
    assertFalse(actualJar.isInUpdateMode());
    assertFalse(actualJar.doubleFilePass);
    assertFalse(actualJar.skipWriting);
    assertTrue(actualJar.addedDirs.isEmpty());
    assertTrue(actualJar.entries.isEmpty());
    assertTrue(actualJar.getUseLanguageEnodingFlag());
    assertTrue(actualJar.isCompress());
    assertTrue(actualJar.isFirstPass());
  }

  /**
   * Test {@link Jar#addMetainf(ZipFileSet)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link ZipFileSet#ZipFileSet()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Jar#addMetainf(ZipFileSet)}
   */
  @Test
  public void testAddMetainf_givenProject_whenZipFileSetProjectIsProject() {
    // Arrange
    Jar jar = new Jar();

    ZipFileSet fs = new ZipFileSet();
    fs.setProject(new Project());

    // Act
    jar.addMetainf(fs);

    // Assert
    assertEquals("META-INF/", fs.getPrefix());
  }

  /**
   * Test {@link Jar#addMetainf(ZipFileSet)}.
   * <ul>
   *   <li>When {@link ZipFileSet#ZipFileSet()}.</li>
   *   <li>Then {@link ZipFileSet#ZipFileSet()} Prefix is {@code META-INF/}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jar#addMetainf(ZipFileSet)}
   */
  @Test
  public void testAddMetainf_whenZipFileSet_thenZipFileSetPrefixIsMetaInf() {
    // Arrange
    Jar jar = new Jar();
    ZipFileSet fs = new ZipFileSet();

    // Act
    jar.addMetainf(fs);

    // Assert
    assertEquals("META-INF/", fs.getPrefix());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Jar#setFlattenAttributes(boolean)}
   *   <li>{@link Jar#setIndex(boolean)}
   *   <li>{@link Jar#setIndexMetaInf(boolean)}
   *   <li>{@link Jar#setManifestEncoding(String)}
   *   <li>{@link Jar#setMergeClassPathAttributes(boolean)}
   *   <li>{@link Jar#getIndexJarsMapper()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Jar jar = new Jar();

    // Act
    jar.setFlattenAttributes(true);
    jar.setIndex(true);
    jar.setIndexMetaInf(true);
    jar.setManifestEncoding(Manifest.JAR_ENCODING);
    jar.setMergeClassPathAttributes(true);

    // Assert
    assertNull(jar.getIndexJarsMapper());
  }

  /**
   * Test {@link Jar#initZipOutputStream(ZipOutputStream)}.
   * <p>
   * Method under test: {@link Jar#initZipOutputStream(ZipOutputStream)}
   */
  @Test
  public void testInitZipOutputStream() throws IOException, BuildException {
    // Arrange
    Jar jar = new Jar();

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.putNextEntry(new ZipEntry("/org/apache/tools/ant/defaultManifest.mf"));

    // Act
    jar.initZipOutputStream(zOut);

    // Assert
    Hashtable<String, String> stringStringMap = jar.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("META-INF/", stringStringMap.get("META-INF/"));
    Hashtable<String, String> stringStringMap2 = jar.entries;
    assertEquals(1, stringStringMap2.size());
    assertEquals("META-INF/MANIFEST.MF", stringStringMap2.get("META-INF/MANIFEST.MF"));
  }

  /**
   * Test {@link Jar#initZipOutputStream(ZipOutputStream)}.
   * <p>
   * Method under test: {@link Jar#initZipOutputStream(ZipOutputStream)}
   */
  @Test
  public void testInitZipOutputStream2() throws IOException, BuildException {
    // Arrange
    Jar jar = new Jar();
    jar.setModificationtime("/org/apache/tools/ant/defaultManifest.mf");

    // Act
    jar.initZipOutputStream(new ZipOutputStream(new ByteArrayOutputStream(1)));

    // Assert
    Hashtable<String, String> stringStringMap = jar.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("META-INF/", stringStringMap.get("META-INF/"));
    Hashtable<String, String> stringStringMap2 = jar.entries;
    assertEquals(1, stringStringMap2.size());
    assertEquals("META-INF/MANIFEST.MF", stringStringMap2.get("META-INF/MANIFEST.MF"));
  }

  /**
   * Test {@link Jar#initZipOutputStream(ZipOutputStream)}.
   * <p>
   * Method under test: {@link Jar#initZipOutputStream(ZipOutputStream)}
   */
  @Test
  public void testInitZipOutputStream3() throws IOException, BuildException {
    // Arrange
    Jar jar = new Jar();

    ZipEntry archiveEntry = new ZipEntry("/org/apache/tools/ant/defaultManifest.mf");
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.putNextEntry(archiveEntry);

    // Act
    jar.initZipOutputStream(zOut);

    // Assert
    Hashtable<String, String> stringStringMap = jar.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("META-INF/", stringStringMap.get("META-INF/"));
    Hashtable<String, String> stringStringMap2 = jar.entries;
    assertEquals(1, stringStringMap2.size());
    assertEquals("META-INF/MANIFEST.MF", stringStringMap2.get("META-INF/MANIFEST.MF"));
  }

  /**
   * Test {@link Jar#initZipOutputStream(ZipOutputStream)}.
   * <p>
   * Method under test: {@link Jar#initZipOutputStream(ZipOutputStream)}
   */
  @Test
  public void testInitZipOutputStream4() throws IOException, BuildException {
    // Arrange
    Jar jar = new Jar();

    ZipEntry archiveEntry = new ZipEntry("/org/apache/tools/ant/defaultManifest.mf");
    archiveEntry.setSize(3L);
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.putNextEntry(archiveEntry);

    // Act
    jar.initZipOutputStream(zOut);

    // Assert
    Hashtable<String, String> stringStringMap = jar.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("META-INF/", stringStringMap.get("META-INF/"));
    Hashtable<String, String> stringStringMap2 = jar.entries;
    assertEquals(1, stringStringMap2.size());
    assertEquals("META-INF/MANIFEST.MF", stringStringMap2.get("META-INF/MANIFEST.MF"));
  }

  /**
   * Test {@link Jar#initZipOutputStream(ZipOutputStream)}.
   * <ul>
   *   <li>Given {@code Always}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jar#initZipOutputStream(ZipOutputStream)}
   */
  @Test
  public void testInitZipOutputStream_givenAlways() throws IOException, BuildException {
    // Arrange
    Jar jar = new Jar();

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.setUseZip64(Zip64Mode.Always);
    zOut.putNextEntry(new ZipEntry("/org/apache/tools/ant/defaultManifest.mf"));

    // Act
    jar.initZipOutputStream(zOut);

    // Assert
    Hashtable<String, String> stringStringMap = jar.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("META-INF/", stringStringMap.get("META-INF/"));
    Hashtable<String, String> stringStringMap2 = jar.entries;
    assertEquals(1, stringStringMap2.size());
    assertEquals("META-INF/MANIFEST.MF", stringStringMap2.get("META-INF/MANIFEST.MF"));
  }

  /**
   * Test {@link Jar#initZipOutputStream(ZipOutputStream)}.
   * <ul>
   *   <li>Given {@code Always}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jar#initZipOutputStream(ZipOutputStream)}
   */
  @Test
  public void testInitZipOutputStream_givenAlways2() throws IOException, BuildException {
    // Arrange
    Jar jar = new Jar();

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.setUseZip64(Zip64Mode.Always);

    // Act
    jar.initZipOutputStream(zOut);

    // Assert
    Hashtable<String, String> stringStringMap = jar.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("META-INF/", stringStringMap.get("META-INF/"));
    Hashtable<String, String> stringStringMap2 = jar.entries;
    assertEquals(1, stringStringMap2.size());
    assertEquals("META-INF/MANIFEST.MF", stringStringMap2.get("META-INF/MANIFEST.MF"));
  }

  /**
   * Test {@link Jar#initZipOutputStream(ZipOutputStream)}.
   * <ul>
   *   <li>Given {@link Jar} (default constructor) FlattenAttributes is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jar#initZipOutputStream(ZipOutputStream)}
   */
  @Test
  public void testInitZipOutputStream_givenJarFlattenAttributesIsTrue() throws IOException, BuildException {
    // Arrange
    Jar jar = new Jar();
    jar.setFlattenAttributes(true);

    // Act
    jar.initZipOutputStream(new ZipOutputStream(new ByteArrayOutputStream(1)));

    // Assert
    Hashtable<String, String> stringStringMap = jar.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("META-INF/", stringStringMap.get("META-INF/"));
    Hashtable<String, String> stringStringMap2 = jar.entries;
    assertEquals(1, stringStringMap2.size());
    assertEquals("META-INF/MANIFEST.MF", stringStringMap2.get("META-INF/MANIFEST.MF"));
  }

  /**
   * Test {@link Jar#initZipOutputStream(ZipOutputStream)}.
   * <ul>
   *   <li>Given {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jar#initZipOutputStream(ZipOutputStream)}
   */
  @Test
  public void testInitZipOutputStream_givenNull() throws IOException, BuildException {
    // Arrange
    Jar jar = new Jar();

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.setCreateUnicodeExtraFields(null);
    zOut.putNextEntry(new ZipEntry("/org/apache/tools/ant/defaultManifest.mf"));

    // Act
    jar.initZipOutputStream(zOut);

    // Assert
    Hashtable<String, String> stringStringMap = jar.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("META-INF/", stringStringMap.get("META-INF/"));
    Hashtable<String, String> stringStringMap2 = jar.entries;
    assertEquals(1, stringStringMap2.size());
    assertEquals("META-INF/MANIFEST.MF", stringStringMap2.get("META-INF/MANIFEST.MF"));
  }

  /**
   * Test {@link Jar#initZipOutputStream(ZipOutputStream)}.
   * <ul>
   *   <li>When {@link ZipOutputStream#ZipOutputStream(OutputStream)} with out is {@link ByteArrayOutputStream#ByteArrayOutputStream(int)}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jar#initZipOutputStream(ZipOutputStream)}
   */
  @Test
  public void testInitZipOutputStream_whenZipOutputStreamWithOutIsByteArrayOutputStream()
      throws IOException, BuildException {
    // Arrange
    Jar jar = new Jar();

    // Act
    jar.initZipOutputStream(new ZipOutputStream(new ByteArrayOutputStream(1)));

    // Assert
    Hashtable<String, String> stringStringMap = jar.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("META-INF/", stringStringMap.get("META-INF/"));
    Hashtable<String, String> stringStringMap2 = jar.entries;
    assertEquals(1, stringStringMap2.size());
    assertEquals("META-INF/MANIFEST.MF", stringStringMap2.get("META-INF/MANIFEST.MF"));
  }

  /**
   * Test {@link Jar#getResourcesToAdd(ResourceCollection[], File, boolean)} with {@code rcs}, {@code zipFile}, {@code needsUpdate}.
   * <p>
   * Method under test: {@link Jar#getResourcesToAdd(ResourceCollection[], File, boolean)}
   */
  @Test
  public void testGetResourcesToAddWithRcsZipFileNeedsUpdate() throws BuildException {
    // Arrange and Act
    ArchiveState actualResourcesToAdd = (new Jar()).getResourcesToAdd(new ResourceCollection[]{Resources.NONE},
        Copy.NULL_FILE_PLACEHOLDER, true);

    // Assert
    Resource[][] resourcesToAdd = actualResourcesToAdd.getResourcesToAdd();
    assertEquals(0, (resourcesToAdd[0]).length);
    assertEquals(1, resourcesToAdd.length);
    assertTrue(actualResourcesToAdd.isOutOfDate());
    assertTrue(actualResourcesToAdd.isWithoutAnyResources());
  }

  /**
   * Test {@link Jar#getResourcesToAdd(ResourceCollection[], File, boolean)} with {@code rcs}, {@code zipFile}, {@code needsUpdate}.
   * <p>
   * Method under test: {@link Jar#getResourcesToAdd(ResourceCollection[], File, boolean)}
   */
  @Test
  public void testGetResourcesToAddWithRcsZipFileNeedsUpdate2() throws BuildException {
    // Arrange
    Jar jar = new Jar();

    // Act
    ArchiveState actualResourcesToAdd = jar.getResourcesToAdd(new ResourceCollection[]{new FileList()},
        Copy.NULL_FILE_PLACEHOLDER, true);

    // Assert
    Resource[][] resourcesToAdd = actualResourcesToAdd.getResourcesToAdd();
    assertEquals(0, (resourcesToAdd[0]).length);
    assertEquals(1, resourcesToAdd.length);
    assertTrue(actualResourcesToAdd.isOutOfDate());
    assertTrue(actualResourcesToAdd.isWithoutAnyResources());
  }

  /**
   * Test {@link Jar#getResourcesToAdd(ResourceCollection[], File, boolean)} with {@code rcs}, {@code zipFile}, {@code needsUpdate}.
   * <p>
   * Method under test: {@link Jar#getResourcesToAdd(ResourceCollection[], File, boolean)}
   */
  @Test
  public void testGetResourcesToAddWithRcsZipFileNeedsUpdate3() throws BuildException {
    // Arrange
    Jar jar = new Jar();

    // Act
    ArchiveState actualResourcesToAdd = jar.getResourcesToAdd(new ResourceCollection[]{Resources.NONE},
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), true);

    // Assert
    Resource[][] resourcesToAdd = actualResourcesToAdd.getResourcesToAdd();
    assertEquals(0, (resourcesToAdd[0]).length);
    assertEquals(1, resourcesToAdd.length);
    assertTrue(actualResourcesToAdd.isOutOfDate());
    assertTrue(actualResourcesToAdd.isWithoutAnyResources());
  }

  /**
   * Test {@link Jar#createEmptyZip(File)}.
   * <p>
   * Method under test: {@link Jar#createEmptyZip(File)}
   */
  @Test
  public void testCreateEmptyZip() throws BuildException {
    // Arrange, Act and Assert
    assertTrue((new Jar()).createEmptyZip(Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Jar#findJarName(String, String[])}.
   * <ul>
   *   <li>When array of {@link String} with {@code Classpath}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jar#findJarName(String, String[])}
   */
  @Test
  public void testFindJarName_whenArrayOfStringWithClasspath_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull(Jar.findJarName("foo.txt", new String[]{"Classpath"}));
  }

  /**
   * Test {@link Jar#findJarName(String, String[])}.
   * <ul>
   *   <li>When array of {@link String} with {@code ./}.</li>
   *   <li>Then return {@code ./}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jar#findJarName(String, String[])}
   */
  @Test
  public void testFindJarName_whenArrayOfStringWithDotSlash_thenReturnDotSlash() {
    // Arrange, Act and Assert
    assertEquals("./", Jar.findJarName("foo.txt", new String[]{"./"}));
  }

  /**
   * Test {@link Jar#findJarName(String, String[])}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code foo.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jar#findJarName(String, String[])}
   */
  @Test
  public void testFindJarName_whenNull_thenReturnFooTxt() {
    // Arrange, Act and Assert
    assertEquals("foo.txt", Jar.findJarName("foo.txt", null));
  }

  /**
   * Test StrictMode {@link StrictMode#getLogLevel()}.
   * <ul>
   *   <li>Given {@link StrictMode#StrictMode()}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link StrictMode#getLogLevel()}
   */
  @Test
  public void testStrictModeGetLogLevel_givenStrictMode_thenReturnOne() {
    // Arrange, Act and Assert
    assertEquals(1, (new StrictMode()).getLogLevel());
  }

  /**
   * Test StrictMode {@link StrictMode#getLogLevel()}.
   * <ul>
   *   <li>Then return three.</li>
   * </ul>
   * <p>
   * Method under test: {@link StrictMode#getLogLevel()}
   */
  @Test
  public void testStrictModeGetLogLevel_thenReturnThree() {
    // Arrange, Act and Assert
    assertEquals(3, (new StrictMode(OnError.POLICY_IGNORE)).getLogLevel());
  }

  /**
   * Test StrictMode {@link StrictMode#getValues()}.
   * <ul>
   *   <li>Then return array of {@link String} with {@link OnError#POLICY_FAIL} and {@link Tar#WARN}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StrictMode#getValues()}
   */
  @Test
  public void testStrictModeGetValues_thenReturnArrayOfStringWithPolicy_failAndWarn() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{OnError.POLICY_FAIL, Tar.WARN, OnError.POLICY_IGNORE},
        (new StrictMode(OnError.POLICY_IGNORE)).getValues());
  }

  /**
   * Test StrictMode {@link StrictMode#StrictMode()}.
   * <p>
   * Method under test: {@link StrictMode#StrictMode()}
   */
  @Test
  public void testStrictModeNewStrictMode() {
    // Arrange and Act
    StrictMode actualStrictMode = new StrictMode();

    // Assert
    assertNull(actualStrictMode.getValue());
    assertEquals(-1, actualStrictMode.getIndex());
  }

  /**
   * Test StrictMode {@link StrictMode#StrictMode(String)}.
   * <ul>
   *   <li>When {@link OnError#POLICY_FAIL}.</li>
   *   <li>Then return Index is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link StrictMode#StrictMode(String)}
   */
  @Test
  public void testStrictModeNewStrictMode_whenPolicy_fail_thenReturnIndexIsZero() {
    // Arrange and Act
    StrictMode actualStrictMode = new StrictMode(OnError.POLICY_FAIL);

    // Assert
    assertEquals(0, actualStrictMode.getIndex());
    assertEquals(1, actualStrictMode.getLogLevel());
    assertEquals(OnError.POLICY_FAIL, actualStrictMode.getValue());
    assertArrayEquals(new String[]{OnError.POLICY_FAIL, Tar.WARN, OnError.POLICY_IGNORE}, actualStrictMode.getValues());
  }
}
