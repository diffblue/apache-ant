package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Hashtable;
import java.util.List;
import java.util.Vector;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.filters.ClassConstants;
import org.apache.tools.ant.filters.ExpandProperties;
import org.apache.tools.ant.filters.HeadFilter;
import org.apache.tools.ant.filters.TokenFilter;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.FilterChain;
import org.apache.tools.ant.types.FilterSet;
import org.apache.tools.ant.types.Mapper;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.mappers.FilterMapper;
import org.apache.tools.ant.types.optional.ScriptMapper;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.JavaConstantResource;
import org.apache.tools.ant.types.resources.JavaResource;
import org.apache.tools.ant.util.ChainedMapper;
import org.apache.tools.ant.util.CompositeMapper;
import org.apache.tools.ant.util.FileNameMapper;
import org.apache.tools.ant.util.FileUtils;
import org.junit.Test;

public class CopyDiffblueTest {
  /**
   * Test new {@link Copy} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Copy}
   */
  @Test
  public void testNewCopy() {
    // Arrange and Act
    Copy actualCopy = new Copy();

    // Assert
    assertNull(actualCopy.destDir);
    assertNull(actualCopy.destFile);
    assertNull(actualCopy.file);
    assertNull(actualCopy.getDescription());
    assertNull(actualCopy.getTaskName());
    assertNull(actualCopy.getTaskType());
    assertNull(actualCopy.getEncoding());
    assertNull(actualCopy.getOutputEncoding());
    assertNull(actualCopy.getProject());
    assertNull(actualCopy.getOwningTarget());
    assertNull(actualCopy.mapperElement);
    assertEquals(3, actualCopy.verbosity);
    assertFalse(actualCopy.getForce());
    assertFalse(actualCopy.getPreserveLastModified());
    assertFalse(actualCopy.isEnableMultipleMapping());
    assertFalse(actualCopy.filtering);
    assertFalse(actualCopy.flatten);
    assertFalse(actualCopy.forceOverwrite);
    assertTrue(actualCopy.completeDirMap.isEmpty());
    assertTrue(actualCopy.dirCopyMap.isEmpty());
    assertTrue(actualCopy.fileCopyMap.isEmpty());
    assertTrue(actualCopy.getFilterChains().isEmpty());
    assertTrue(actualCopy.getFilterSets().isEmpty());
    assertTrue(actualCopy.filesets.isEmpty());
    assertTrue(actualCopy.rcs.isEmpty());
    assertTrue(actualCopy.failonerror);
    assertTrue(actualCopy.includeEmpty);
  }

  /**
   * Test {@link Copy#createFilterChain()}.
   * <p>
   * Method under test: {@link Copy#createFilterChain()}
   */
  @Test
  public void testCreateFilterChain() {
    // Arrange
    Copy copy = new Copy();

    // Act
    FilterChain actualCreateFilterChainResult = copy.createFilterChain();

    // Assert
    Location location = actualCreateFilterChainResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateFilterChainResult.getDescription());
    assertNull(actualCreateFilterChainResult.getProject());
    assertNull(actualCreateFilterChainResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(1, copy.getFilterChains().size());
    assertFalse(actualCreateFilterChainResult.isReference());
    assertTrue(actualCreateFilterChainResult.getFilterReaders().isEmpty());
  }

  /**
   * Test {@link Copy#createFilterSet()}.
   * <p>
   * Method under test: {@link Copy#createFilterSet()}
   */
  @Test
  public void testCreateFilterSet() {
    // Arrange
    Copy copy = new Copy();

    // Act
    FilterSet actualCreateFilterSetResult = copy.createFilterSet();

    // Assert
    assertEquals("@", actualCreateFilterSetResult.getBeginToken());
    assertEquals("@", actualCreateFilterSetResult.getEndToken());
    assertNull(actualCreateFilterSetResult.getDescription());
    assertNull(actualCreateFilterSetResult.getProject());
    assertNull(actualCreateFilterSetResult.getRefid());
    assertEquals(1, copy.getFilterSets().size());
    assertFalse(actualCreateFilterSetResult.isReference());
    assertFalse(actualCreateFilterSetResult.hasFilters());
    assertTrue(actualCreateFilterSetResult.getFilterHash().isEmpty());
    assertTrue(actualCreateFilterSetResult.isRecurse());
  }

  /**
   * Test {@link Copy#setPreserveLastModified(String)} with {@code String}.
   * <ul>
   *   <li>Then not {@link Copy} (default constructor) PreserveLastModified.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#setPreserveLastModified(String)}
   */
  @Test
  public void testSetPreserveLastModifiedWithString_thenNotCopyPreserveLastModified() {
    // Arrange
    Copy copy = new Copy();

    // Act
    copy.setPreserveLastModified("Preserve");

    // Assert that nothing has changed
    assertFalse(copy.getPreserveLastModified());
  }

  /**
   * Test {@link Copy#setPreserveLastModified(String)} with {@code String}.
   * <ul>
   *   <li>When {@code on}.</li>
   *   <li>Then {@link Copy} (default constructor) PreserveLastModified.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#setPreserveLastModified(String)}
   */
  @Test
  public void testSetPreserveLastModifiedWithString_whenOn_thenCopyPreserveLastModified() {
    // Arrange
    Copy copy = new Copy();

    // Act
    copy.setPreserveLastModified("on");

    // Assert
    assertTrue(copy.getPreserveLastModified());
  }

  /**
   * Test {@link Copy#setPreserveLastModified(String)} with {@code String}.
   * <ul>
   *   <li>When {@link Boolean#TRUE} toString.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#setPreserveLastModified(String)}
   */
  @Test
  public void testSetPreserveLastModifiedWithString_whenTrueToString() {
    // Arrange
    Copy copy = new Copy();

    // Act
    copy.setPreserveLastModified(Boolean.TRUE.toString());

    // Assert
    assertTrue(copy.getPreserveLastModified());
  }

  /**
   * Test {@link Copy#setPreserveLastModified(String)} with {@code String}.
   * <ul>
   *   <li>When {@code yes}.</li>
   *   <li>Then {@link Copy} (default constructor) PreserveLastModified.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#setPreserveLastModified(String)}
   */
  @Test
  public void testSetPreserveLastModifiedWithString_whenYes_thenCopyPreserveLastModified() {
    // Arrange
    Copy copy = new Copy();

    // Act
    copy.setPreserveLastModified("yes");

    // Assert
    assertTrue(copy.getPreserveLastModified());
  }

  /**
   * Test {@link Copy#setVerbose(boolean)}.
   * <ul>
   *   <li>When {@code false}.</li>
   *   <li>Then {@link Copy} (default constructor) {@link Copy#verbosity} is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#setVerbose(boolean)}
   */
  @Test
  public void testSetVerbose_whenFalse_thenCopyVerbosityIsThree() {
    // Arrange
    Copy copy = new Copy();

    // Act
    copy.setVerbose(false);

    // Assert that nothing has changed
    assertEquals(3, copy.verbosity);
  }

  /**
   * Test {@link Copy#setVerbose(boolean)}.
   * <ul>
   *   <li>When {@code true}.</li>
   *   <li>Then {@link Copy} (default constructor) {@link Copy#verbosity} is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#setVerbose(boolean)}
   */
  @Test
  public void testSetVerbose_whenTrue_thenCopyVerbosityIsTwo() {
    // Arrange
    Copy copy = new Copy();

    // Act
    copy.setVerbose(true);

    // Assert
    assertEquals(2, copy.verbosity);
  }

  /**
   * Test {@link Copy#addFileset(FileSet)}.
   * <p>
   * Method under test: {@link Copy#addFileset(FileSet)}
   */
  @Test
  public void testAddFileset() {
    // Arrange
    Copy copy = new Copy();
    FileSet set = new FileSet();

    // Act
    copy.addFileset(set);

    // Assert
    Vector<ResourceCollection> resourceCollectionList = copy.filesets;
    assertEquals(1, resourceCollectionList.size());
    Vector<ResourceCollection> resourceCollectionList2 = copy.rcs;
    assertEquals(1, resourceCollectionList2.size());
    assertTrue(copy.getFilterSets().isEmpty());
    assertSame(set, resourceCollectionList.get(0));
    assertSame(set, resourceCollectionList2.get(0));
  }

  /**
   * Test {@link Copy#add(FileNameMapper)} with {@code fileNameMapper}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#add(FileNameMapper)}
   */
  @Test
  public void testAddWithFileNameMapper_givenCopyAddCutDirsMapper_thenThrowBuildException() {
    // Arrange
    Copy copy = new Copy();
    copy.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> copy.add(new CutDirsMapper()));
  }

  /**
   * Test {@link Copy#add(FileNameMapper)} with {@code fileNameMapper}.
   * <ul>
   *   <li>Then {@link Copy} (default constructor) {@link Copy#mapperElement} Implementation {@link CompositeMapper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#add(FileNameMapper)}
   */
  @Test
  public void testAddWithFileNameMapper_thenCopyMapperElementImplementationCompositeMapper() throws BuildException {
    // Arrange
    Copy copy = new Copy();
    CutDirsMapper fileNameMapper = new CutDirsMapper();

    // Act
    copy.add(fileNameMapper);

    // Assert
    Mapper mapper = copy.mapperElement;
    FileNameMapper implementation = mapper.getImplementation();
    assertTrue(implementation instanceof CompositeMapper);
    assertNull(mapper.getDescription());
    assertNull(mapper.getProject());
    assertNull(mapper.getRefid());
    List<FileNameMapper> mappers = ((CompositeMapper) implementation).getMappers();
    assertEquals(1, mappers.size());
    assertFalse(mapper.isReference());
    assertSame(fileNameMapper, mappers.get(0));
  }

  /**
   * Test {@link Copy#add(ResourceCollection)} with {@code res}.
   * <p>
   * Method under test: {@link Copy#add(ResourceCollection)}
   */
  @Test
  public void testAddWithRes() {
    // Arrange
    Copy copy = new Copy();
    Path res = Path.systemBootClasspath;

    // Act
    copy.add(res);

    // Assert
    Vector<ResourceCollection> resourceCollectionList = copy.filesets;
    assertEquals(1, resourceCollectionList.size());
    Vector<ResourceCollection> resourceCollectionList2 = copy.rcs;
    assertEquals(1, resourceCollectionList2.size());
    assertTrue(copy.getFilterChains().isEmpty());
    assertTrue(copy.getFilterSets().isEmpty());
    Path path = res.systemBootClasspath;
    assertSame(path, resourceCollectionList.get(0));
    assertSame(path, resourceCollectionList2.get(0));
  }

  /**
   * Test {@link Copy#setEncoding(String)}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) OutputEncoding is {@code foo}.</li>
   *   <li>Then {@link Copy} (default constructor) OutputEncoding is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#setEncoding(String)}
   */
  @Test
  public void testSetEncoding_givenCopyOutputEncodingIsFoo_thenCopyOutputEncodingIsFoo() {
    // Arrange
    Copy copy = new Copy();
    copy.setOutputEncoding("foo");

    // Act
    copy.setEncoding(Manifest.JAR_ENCODING);

    // Assert
    assertEquals("foo", copy.getOutputEncoding());
    assertEquals(Manifest.JAR_ENCODING, copy.getEncoding());
  }

  /**
   * Test {@link Copy#setEncoding(String)}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor).</li>
   *   <li>Then {@link Copy} (default constructor) OutputEncoding is {@link Manifest#JAR_ENCODING}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#setEncoding(String)}
   */
  @Test
  public void testSetEncoding_givenCopy_thenCopyOutputEncodingIsJar_encoding() {
    // Arrange
    Copy copy = new Copy();

    // Act
    copy.setEncoding(Manifest.JAR_ENCODING);

    // Assert
    assertEquals(Manifest.JAR_ENCODING, copy.getEncoding());
    assertEquals(Manifest.JAR_ENCODING, copy.getOutputEncoding());
  }

  /**
   * Test {@link Copy#createMapper()}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#createMapper()}
   */
  @Test
  public void testCreateMapper_givenCopyAddCutDirsMapper_thenThrowBuildException() throws BuildException {
    // Arrange
    Copy copy = new Copy();
    copy.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> copy.createMapper());
  }

  /**
   * Test {@link Copy#createMapper()}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor).</li>
   *   <li>Then {@link Copy} (default constructor) {@link Copy#mapperElement} Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#createMapper()}
   */
  @Test
  public void testCreateMapper_givenCopy_thenCopyMapperElementDescriptionIsNull() throws BuildException {
    // Arrange
    Copy copy = new Copy();

    // Act
    Mapper actualCreateMapperResult = copy.createMapper();

    // Assert
    Mapper mapper = copy.mapperElement;
    assertNull(mapper.getDescription());
    assertNull(mapper.getProject());
    assertNull(mapper.getRefid());
    assertFalse(mapper.isReference());
    assertSame(copy.mapperElement, actualCreateMapperResult);
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Copy#setEnableMultipleMappings(boolean)}
   *   <li>{@link Copy#setFailOnError(boolean)}
   *   <li>{@link Copy#setFile(File)}
   *   <li>{@link Copy#setFiltering(boolean)}
   *   <li>{@link Copy#setFlatten(boolean)}
   *   <li>{@link Copy#setForce(boolean)}
   *   <li>{@link Copy#setGranularity(long)}
   *   <li>{@link Copy#setIncludeEmptyDirs(boolean)}
   *   <li>{@link Copy#setOutputEncoding(String)}
   *   <li>{@link Copy#setOverwrite(boolean)}
   *   <li>{@link Copy#setPreserveLastModified(boolean)}
   *   <li>{@link Copy#setQuiet(boolean)}
   *   <li>{@link Copy#setTodir(File)}
   *   <li>{@link Copy#setTofile(File)}
   *   <li>{@link Copy#getEncoding()}
   *   <li>{@link Copy#getFileUtils()}
   *   <li>{@link Copy#getFilterChains()}
   *   <li>{@link Copy#getFilterSets()}
   *   <li>{@link Copy#getForce()}
   *   <li>{@link Copy#getOutputEncoding()}
   *   <li>{@link Copy#getPreserveLastModified()}
   *   <li>{@link Copy#isEnableMultipleMapping()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Copy copy = new Copy();

    // Act
    copy.setEnableMultipleMappings(true);
    copy.setFailOnError(true);
    copy.setFile(Copy.NULL_FILE_PLACEHOLDER);
    copy.setFiltering(true);
    copy.setFlatten(true);
    copy.setForce(true);
    copy.setGranularity(WaitFor.ONE_MILLISECOND);
    copy.setIncludeEmptyDirs(true);
    copy.setOutputEncoding(Manifest.JAR_ENCODING);
    copy.setOverwrite(true);
    copy.setPreserveLastModified(true);
    copy.setQuiet(true);
    copy.setTodir(Copy.NULL_FILE_PLACEHOLDER);
    copy.setTofile(Copy.NULL_FILE_PLACEHOLDER);
    String actualEncoding = copy.getEncoding();
    FileUtils actualFileUtils = copy.getFileUtils();
    Vector<FilterChain> actualFilterChains = copy.getFilterChains();
    Vector<FilterSet> actualFilterSets = copy.getFilterSets();
    boolean actualForce = copy.getForce();
    String actualOutputEncoding = copy.getOutputEncoding();
    boolean actualPreserveLastModified = copy.getPreserveLastModified();
    boolean actualIsEnableMultipleMappingResult = copy.isEnableMultipleMapping();

    // Assert
    assertEquals("UTF8", actualFileUtils.getDefaultEncoding());
    assertNull(actualEncoding);
    assertTrue(actualFilterChains.isEmpty());
    assertTrue(actualFilterSets.isEmpty());
    assertTrue(actualForce);
    assertTrue(actualPreserveLastModified);
    assertTrue(actualIsEnableMultipleMappingResult);
    assertEquals(Manifest.JAR_ENCODING, actualOutputEncoding);
    assertEquals(WaitFor.ONE_SECOND, actualFileUtils.getFileTimestampGranularity());
  }

  /**
   * Test {@link Copy#execute()}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#execute()}
   */
  @Test
  public void testExecute_givenCopyAddFilesetFileSet_thenThrowBuildException() throws BuildException {
    // Arrange
    Copy copy = new Copy();
    copy.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> copy.execute());
  }

  /**
   * Test {@link Copy#execute()}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) addFileset {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#execute()}
   */
  @Test
  public void testExecute_givenCopyAddFilesetNull_thenThrowBuildException() throws BuildException {
    // Arrange
    Copy copy = new Copy();
    copy.addFileset(new FileSet());
    copy.setTofile(Copy.NULL_FILE_PLACEHOLDER);
    copy.addFileset(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> copy.execute());
  }

  /**
   * Test {@link Copy#execute()}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) File is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#execute()}
   */
  @Test
  public void testExecute_givenCopyFileIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    Copy copy = new Copy();
    copy.setFile(Copy.NULL_FILE_PLACEHOLDER);
    copy.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> copy.execute());
  }

  /**
   * Test {@link Copy#execute()}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) File is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#execute()}
   */
  @Test
  public void testExecute_givenCopyFileIsNull_file_placeholder_thenThrowBuildException2() throws BuildException {
    // Arrange
    Copy copy = new Copy();
    copy.setFile(Copy.NULL_FILE_PLACEHOLDER);
    copy.addFileset(new FileSet());
    copy.setTofile(Copy.NULL_FILE_PLACEHOLDER);
    copy.addFileset(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> copy.execute());
  }

  /**
   * Test {@link Copy#execute()}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) Todir is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#execute()}
   */
  @Test
  public void testExecute_givenCopyTodirIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    Copy copy = new Copy();
    copy.setTodir(Copy.NULL_FILE_PLACEHOLDER);
    copy.addFileset(new FileSet());
    copy.setTofile(Copy.NULL_FILE_PLACEHOLDER);
    copy.addFileset(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> copy.execute());
  }

  /**
   * Test {@link Copy#execute()}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) Tofile is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#execute()}
   */
  @Test
  public void testExecute_givenCopyTofileIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    Copy copy = new Copy();
    copy.setTofile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> copy.execute());
  }

  /**
   * Test {@link Copy#execute()}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#execute()}
   */
  @Test
  public void testExecute_givenCopy_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Copy()).execute());
  }

  /**
   * Test {@link Copy#validateAttributes()}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenCopyAddFilesetFileSet_thenThrowBuildException() throws BuildException {
    // Arrange
    Copy copy = new Copy();
    copy.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> copy.validateAttributes());
  }

  /**
   * Test {@link Copy#validateAttributes()}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) addFileset {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenCopyAddFilesetNull_thenThrowBuildException() throws BuildException {
    // Arrange
    Copy copy = new Copy();
    copy.addFileset(new FileSet());
    copy.setTofile(Copy.NULL_FILE_PLACEHOLDER);
    copy.addFileset(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> copy.validateAttributes());
  }

  /**
   * Test {@link Copy#validateAttributes()}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) File is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenCopyFileIsNull_file_placeholder() throws BuildException {
    // Arrange
    Copy copy = new Copy();
    copy.setFile(Copy.NULL_FILE_PLACEHOLDER);
    copy.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> copy.validateAttributes());
  }

  /**
   * Test {@link Copy#validateAttributes()}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) File is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenCopyFileIsNull_file_placeholder2() throws BuildException {
    // Arrange
    Copy copy = new Copy();
    copy.setFile(Copy.NULL_FILE_PLACEHOLDER);
    copy.addFileset(new FileSet());
    copy.setTofile(Copy.NULL_FILE_PLACEHOLDER);
    copy.addFileset(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> copy.validateAttributes());
  }

  /**
   * Test {@link Copy#validateAttributes()}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) Todir is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenCopyTodirIsNull_file_placeholder() throws BuildException {
    // Arrange
    Copy copy = new Copy();
    copy.setTodir(Copy.NULL_FILE_PLACEHOLDER);
    copy.addFileset(new FileSet());
    copy.setTofile(Copy.NULL_FILE_PLACEHOLDER);
    copy.addFileset(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> copy.validateAttributes());
  }

  /**
   * Test {@link Copy#validateAttributes()}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenCopy_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Copy()).validateAttributes());
  }

  /**
   * Test {@link Copy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <p>
   * Method under test: {@link Copy#scan(File, File, String[], String[])}
   */
  @Test
  public void testScanWithFromDirToDirFilesDirs() {
    // Arrange
    Copy copy = new Copy();

    // Act
    copy.scan(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), Copy.NULL_FILE_PLACEHOLDER,
        new String[]{"."}, new String[]{"Dirs"});

    // Assert
    Hashtable<String, String[]> stringStringArrayMap = copy.fileCopyMap;
    assertEquals(1, stringStringArrayMap.size());
    assertArrayEquals(new String[]{"/NULL_FILE/."},
        stringStringArrayMap.get(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt", ".").toString()));
  }

  /**
   * Test {@link Copy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <p>
   * Method under test: {@link Copy#scan(File, File, String[], String[])}
   */
  @Test
  public void testScanWithFromDirToDirFilesDirs2() {
    // Arrange
    Copy copy = new Copy();

    // Act
    copy.scan(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), Copy.NULL_FILE_PLACEHOLDER,
        new String[]{".."}, new String[]{"Dirs"});

    // Assert
    Hashtable<String, String[]> stringStringArrayMap = copy.fileCopyMap;
    assertEquals(1, stringStringArrayMap.size());
    assertArrayEquals(new String[]{"/NULL_FILE/.."},
        stringStringArrayMap.get(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt", "..").toString()));
  }

  /**
   * Test {@link Copy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(File, File, String[], String[])}
   */
  @Test
  public void testScanWithFromDirToDirFilesDirs_givenCopyAddCutDirsMapper() {
    // Arrange
    Copy copy = new Copy();
    copy.add(new CutDirsMapper());

    // Act
    copy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(copy.dirCopyMap.isEmpty());
    assertTrue(copy.fileCopyMap.isEmpty());
  }

  /**
   * Test {@link Copy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) add {@link FilterMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(File, File, String[], String[])}
   */
  @Test
  public void testScanWithFromDirToDirFilesDirs_givenCopyAddFilterMapper() {
    // Arrange
    Copy copy = new Copy();
    copy.add(new FilterMapper());

    // Act
    copy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(copy.dirCopyMap.isEmpty());
    assertTrue(copy.fileCopyMap.isEmpty());
  }

  /**
   * Test {@link Copy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) add {@code null}.</li>
   *   <li>When array of {@link String} with {@code Files}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(File, File, String[], String[])}
   */
  @Test
  public void testScanWithFromDirToDirFilesDirs_givenCopyAddNull_whenArrayOfStringWithFiles() {
    // Arrange
    Copy copy = new Copy();
    copy.setOverwrite(true);
    copy.add((FileNameMapper) null);

    // Act
    copy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(copy.dirCopyMap.isEmpty());
    assertTrue(copy.fileCopyMap.isEmpty());
  }

  /**
   * Test {@link Copy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) add {@link ScriptMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(File, File, String[], String[])}
   */
  @Test
  public void testScanWithFromDirToDirFilesDirs_givenCopyAddScriptMapper() {
    // Arrange
    Copy copy = new Copy();
    copy.add(new ScriptMapper());

    // Act
    copy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(copy.dirCopyMap.isEmpty());
    assertTrue(copy.fileCopyMap.isEmpty());
  }

  /**
   * Test {@link Copy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) Flatten is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(File, File, String[], String[])}
   */
  @Test
  public void testScanWithFromDirToDirFilesDirs_givenCopyFlattenIsTrue() {
    // Arrange
    Copy copy = new Copy();
    copy.setFlatten(true);

    // Act
    copy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(copy.dirCopyMap.isEmpty());
    assertTrue(copy.fileCopyMap.isEmpty());
  }

  /**
   * Test {@link Copy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(File, File, String[], String[])}
   */
  @Test
  public void testScanWithFromDirToDirFilesDirs_givenCopyProjectIsProject() {
    // Arrange
    Copy copy = new Copy();
    copy.setProject(new Project());
    copy.add(new CutDirsMapper());

    // Act
    copy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(copy.dirCopyMap.isEmpty());
    assertTrue(copy.fileCopyMap.isEmpty());
  }

  /**
   * Test {@link Copy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor).</li>
   *   <li>When array of {@link String} with {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(File, File, String[], String[])}
   */
  @Test
  public void testScanWithFromDirToDirFilesDirs_givenCopy_whenArrayOfStringWithDot() {
    // Arrange
    Copy copy = new Copy();

    // Act
    copy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{"."}, new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(copy.dirCopyMap.isEmpty());
    assertTrue(copy.fileCopyMap.isEmpty());
  }

  /**
   * Test {@link Copy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor).</li>
   *   <li>When array of {@link String} with {@code .} and {@code NULL_FILE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(File, File, String[], String[])}
   */
  @Test
  public void testScanWithFromDirToDirFilesDirs_givenCopy_whenArrayOfStringWithDotAndNullFile() {
    // Arrange
    Copy copy = new Copy();

    // Act
    copy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{".", "NULL_FILE"},
        new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(copy.dirCopyMap.isEmpty());
    assertTrue(copy.fileCopyMap.isEmpty());
  }

  /**
   * Test {@link Copy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor).</li>
   *   <li>When array of {@link String} with {@code .} and {@code NULL_FILE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(File, File, String[], String[])}
   */
  @Test
  public void testScanWithFromDirToDirFilesDirs_givenCopy_whenArrayOfStringWithDotAndNullFile2() {
    // Arrange
    Copy copy = new Copy();

    // Act
    copy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{".", "NULL_FILE", "."},
        new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(copy.dirCopyMap.isEmpty());
    assertTrue(copy.fileCopyMap.isEmpty());
  }

  /**
   * Test {@link Copy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor).</li>
   *   <li>When array of {@link String} with {@code .} and {@code /NULL_FILE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(File, File, String[], String[])}
   */
  @Test
  public void testScanWithFromDirToDirFilesDirs_givenCopy_whenArrayOfStringWithDotAndNullFile3() {
    // Arrange
    Copy copy = new Copy();

    // Act
    copy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{".", "/NULL_FILE"},
        new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(copy.dirCopyMap.isEmpty());
    assertTrue(copy.fileCopyMap.isEmpty());
  }

  /**
   * Test {@link Copy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor).</li>
   *   <li>When array of {@link String} with {@code ..}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(File, File, String[], String[])}
   */
  @Test
  public void testScanWithFromDirToDirFilesDirs_givenCopy_whenArrayOfStringWithDotDot() {
    // Arrange
    Copy copy = new Copy();

    // Act
    copy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{".."}, new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(copy.dirCopyMap.isEmpty());
    assertTrue(copy.fileCopyMap.isEmpty());
  }

  /**
   * Test {@link Copy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor).</li>
   *   <li>When array of {@link String} with empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(File, File, String[], String[])}
   */
  @Test
  public void testScanWithFromDirToDirFilesDirs_givenCopy_whenArrayOfStringWithEmptyString() {
    // Arrange
    Copy copy = new Copy();

    // Act
    copy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{""}, new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(copy.dirCopyMap.isEmpty());
    assertTrue(copy.fileCopyMap.isEmpty());
  }

  /**
   * Test {@link Copy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor).</li>
   *   <li>When array of {@link String} with {@code Files}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(File, File, String[], String[])}
   */
  @Test
  public void testScanWithFromDirToDirFilesDirs_givenCopy_whenArrayOfStringWithFiles() {
    // Arrange
    Copy copy = new Copy();

    // Act
    copy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(copy.dirCopyMap.isEmpty());
    assertTrue(copy.fileCopyMap.isEmpty());
  }

  /**
   * Test {@link Copy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor).</li>
   *   <li>When array of {@link String} with {@code /NULL_FILE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(File, File, String[], String[])}
   */
  @Test
  public void testScanWithFromDirToDirFilesDirs_givenCopy_whenArrayOfStringWithNullFile() {
    // Arrange
    Copy copy = new Copy();

    // Act
    copy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{"/NULL_FILE"}, new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(copy.dirCopyMap.isEmpty());
    assertTrue(copy.fileCopyMap.isEmpty());
  }

  /**
   * Test {@link Copy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor).</li>
   *   <li>When empty array of {@link String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(File, File, String[], String[])}
   */
  @Test
  public void testScanWithFromDirToDirFilesDirs_givenCopy_whenEmptyArrayOfString() {
    // Arrange
    Copy copy = new Copy();

    // Act
    copy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{}, new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(copy.dirCopyMap.isEmpty());
    assertTrue(copy.fileCopyMap.isEmpty());
  }

  /**
   * Test {@link Copy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(File, File, String[], String[])}
   */
  @Test
  public void testScanWithFromDirToDirFilesDirs_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Copy copy = new Copy();
    copy.setProject(project);
    copy.add(new CutDirsMapper());

    // Act
    copy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, new String[]{"Dirs"});

    // Assert that nothing has changed
    assertTrue(copy.dirCopyMap.isEmpty());
    assertTrue(copy.fileCopyMap.isEmpty());
  }

  /**
   * Test {@link Copy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>Then {@link Copy} (default constructor) {@link Copy#dirCopyMap} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(File, File, String[], String[])}
   */
  @Test
  public void testScanWithFromDirToDirFilesDirs_thenCopyDirCopyMapSizeIsOne() {
    // Arrange
    Copy copy = new Copy();
    copy.setOverwrite(true);

    // Act
    copy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, new String[]{"Dirs"});

    // Assert
    Hashtable<String, String[]> stringStringArrayMap = copy.dirCopyMap;
    assertEquals(1, stringStringArrayMap.size());
    Hashtable<String, String[]> stringStringArrayMap2 = copy.fileCopyMap;
    assertEquals(1, stringStringArrayMap2.size());
    assertArrayEquals(new String[]{"/NULL_FILE/Dirs"}, stringStringArrayMap.get("/NULL_FILE/Dirs"));
    assertArrayEquals(new String[]{"/NULL_FILE/Files"}, stringStringArrayMap2.get("/NULL_FILE/Files"));
  }

  /**
   * Test {@link Copy#scan(File, File, String[], String[])} with {@code fromDir}, {@code toDir}, {@code files}, {@code dirs}.
   * <ul>
   *   <li>Then {@link Copy} (default constructor) {@link Copy#dirCopyMap} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(File, File, String[], String[])}
   */
  @Test
  public void testScanWithFromDirToDirFilesDirs_thenCopyDirCopyMapSizeIsOne2() {
    // Arrange
    Copy copy = new Copy();
    copy.setOverwrite(true);
    copy.add(new FilterMapper());

    // Act
    copy.scan(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}, new String[]{"Dirs"});

    // Assert
    Hashtable<String, String[]> stringStringArrayMap = copy.dirCopyMap;
    assertEquals(1, stringStringArrayMap.size());
    Hashtable<String, String[]> stringStringArrayMap2 = copy.fileCopyMap;
    assertEquals(1, stringStringArrayMap2.size());
    assertArrayEquals(new String[]{"/NULL_FILE/Dirs"}, stringStringArrayMap.get("/NULL_FILE/Dirs"));
    assertArrayEquals(new String[]{"/NULL_FILE/Files"}, stringStringArrayMap2.get("/NULL_FILE/Files"));
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(copy.scan(new Resource[]{new Resource("(anonymous)")}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir2() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertEquals(1, copy
        .scan(new Resource[]{new Resource("(anonymous)", true, WaitFor.ONE_MILLISECOND)}, Copy.NULL_FILE_PLACEHOLDER)
        .size());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir3() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(
        copy.scan(new Resource[]{new FileResource(Copy.NULL_FILE_PLACEHOLDER)}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir4() {
    // Arrange
    Copy copy = new Copy();
    copy.add(new CutDirsMapper());

    // Act and Assert
    assertTrue(copy.scan(new Resource[]{new Resource("(anonymous)")}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir5() {
    // Arrange
    Copy copy = new Copy();
    copy.add(new CutDirsMapper());

    // Act and Assert
    assertTrue(
        copy.scan(new Resource[]{new FileResource(Copy.NULL_FILE_PLACEHOLDER)}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir6() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(copy.scan(new Resource[]{new Resource("")}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir7() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertEquals(1,
        copy.scan(new Resource[]{new Resource(".", true, WaitFor.ONE_MILLISECOND)}, Copy.NULL_FILE_PLACEHOLDER).size());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir8() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(copy.scan(new Resource[]{new Resource("..", true, WaitFor.ONE_MILLISECOND)}, Copy.NULL_FILE_PLACEHOLDER)
        .isEmpty());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir9() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertEquals(1,
        copy.scan(new Resource[]{new Resource("/NULL_FILE", true, WaitFor.ONE_MILLISECOND)}, Copy.NULL_FILE_PLACEHOLDER)
            .size());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir10() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertEquals(1,
        copy.scan(new Resource[]{new Resource("(anonymous)", true, Resource.UNKNOWN_SIZE)}, Copy.NULL_FILE_PLACEHOLDER)
            .size());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir11() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertEquals(1, copy.scan(new Resource[]{new Resource("(anonymous)", true, WaitFor.ONE_MILLISECOND)}, null).size());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir12() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertEquals(1,
        copy.scan(
            new Resource[]{new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())},
            Copy.NULL_FILE_PLACEHOLDER).size());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir13() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(copy.scan(new Resource[]{new JavaResource("(anonymous)", null)}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir14() {
    // Arrange
    Copy copy = new Copy();
    JavaConstantResource javaConstantResource = new JavaConstantResource();

    // Act and Assert
    assertTrue(copy.scan(new Resource[]{javaConstantResource, new Resource()}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir_givenCopyAddCutDirsMapper() {
    // Arrange
    Copy copy = new Copy();
    copy.add(new CutDirsMapper());

    // Act and Assert
    assertTrue(copy.scan(new Resource[]{new Resource()}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) add {@link FilterMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir_givenCopyAddFilterMapper() {
    // Arrange
    Copy copy = new Copy();
    copy.add(new FilterMapper());

    // Act and Assert
    assertTrue(copy.scan(new Resource[]{new Resource("(anonymous)")}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) add {@link ScriptMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir_givenCopyAddScriptMapper() {
    // Arrange
    Copy copy = new Copy();
    copy.add(new ScriptMapper());

    // Act and Assert
    assertTrue(copy.scan(new Resource[]{new Resource()}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) Flatten is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir_givenCopyFlattenIsTrue() {
    // Arrange
    Copy copy = new Copy();
    copy.setFlatten(true);

    // Act and Assert
    assertTrue(copy.scan(new Resource[]{new Resource()}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) Overwrite is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir_givenCopyOverwriteIsTrue() {
    // Arrange
    Copy copy = new Copy();
    copy.setOverwrite(true);

    // Act and Assert
    assertTrue(copy.scan(new Resource[]{new Resource()}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir_givenCopyProjectIsProject() {
    // Arrange
    Copy copy = new Copy();
    copy.setProject(new Project());
    copy.add(new CutDirsMapper());

    // Act and Assert
    assertTrue(copy.scan(new Resource[]{new Resource()}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Copy copy = new Copy();
    copy.setProject(project);
    copy.add(new CutDirsMapper());

    // Act and Assert
    assertTrue(copy.scan(new Resource[]{new Resource()}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <ul>
   *   <li>When array of {@link Resource} with {@link JavaConstantResource} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir_whenArrayOfResourceWithJavaConstantResource() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(copy.scan(new Resource[]{new JavaConstantResource()}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <ul>
   *   <li>When array of {@link Resource} with {@code null}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir_whenArrayOfResourceWithNull_thenReturnEmpty() {
    // Arrange, Act and Assert
    assertTrue((new Copy()).scan(new Resource[]{null}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <ul>
   *   <li>When array of {@link Resource} with {@link Resource#Resource(String)} with name is {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir_whenArrayOfResourceWithResourceWithNameIsDot() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(copy.scan(new Resource[]{new Resource(".")}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <ul>
   *   <li>When array of {@link Resource} with {@link Resource#Resource(String)} with name is {@code ..}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir_whenArrayOfResourceWithResourceWithNameIsDotDot() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(copy.scan(new Resource[]{new Resource("..")}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test {@link Copy#scan(Resource[], File)} with {@code fromResources}, {@code toDir}.
   * <ul>
   *   <li>When array of {@link Resource} with {@link Resource#Resource()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#scan(Resource[], File)}
   */
  @Test
  public void testScanWithFromResourcesToDir_whenArrayOfResourceWithResource_thenReturnEmpty() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(copy.scan(new Resource[]{new Resource()}, Copy.NULL_FILE_PLACEHOLDER).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(copy.buildMap(new Resource[]{new Resource("dirs must be set to a positive number")},
        Copy.NULL_FILE_PLACEHOLDER, new CutDirsMapper()).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper2() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(copy
        .buildMap(new Resource[]{new Resource("dirs must be set to a positive number", true, WaitFor.ONE_MILLISECOND)},
            Copy.NULL_FILE_PLACEHOLDER, new CutDirsMapper())
        .isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper3() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(copy.buildMap(new Resource[]{new FileResource(Copy.NULL_FILE_PLACEHOLDER)}, Copy.NULL_FILE_PLACEHOLDER,
        new CutDirsMapper()).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper4() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(copy
        .buildMap(new Resource[]{new FileResource(Copy.NULL_FILE_PLACEHOLDER, "dirs must be set to a positive number")},
            Copy.NULL_FILE_PLACEHOLDER, new CutDirsMapper())
        .isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper5() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(
        copy.buildMap(new Resource[]{new JavaConstantResource()}, Copy.NULL_FILE_PLACEHOLDER, new CutDirsMapper())
            .isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper6() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(copy.buildMap(new Resource[]{new Resource("dirs must be set to a positive number")},
        Copy.NULL_FILE_PLACEHOLDER, new FilterMapper()).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper7() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(copy
        .buildMap(new Resource[]{new Resource("dirs must be set to a positive number", true, Resource.UNKNOWN_SIZE)},
            Copy.NULL_FILE_PLACEHOLDER, new CutDirsMapper())
        .isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper8() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(copy.buildMap(new Resource[]{new FileResource(Copy.NULL_FILE_PLACEHOLDER)}, Copy.NULL_FILE_PLACEHOLDER,
        new FilterMapper()).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper9() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(copy.buildMap(new Resource[]{new FileResource((File) null, "dirs must be set to a positive number")},
        Copy.NULL_FILE_PLACEHOLDER, new CutDirsMapper()).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper10() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(copy.buildMap(new Resource[]{new FileResource(Copy.NULL_FILE_PLACEHOLDER, ".")},
        Copy.NULL_FILE_PLACEHOLDER, new CutDirsMapper()).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper11() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(copy.buildMap(new Resource[]{new FileResource(Copy.NULL_FILE_PLACEHOLDER, "..")},
        Copy.NULL_FILE_PLACEHOLDER, new CutDirsMapper()).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper12() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(
        copy.buildMap(new Resource[]{new JavaConstantResource()}, Copy.NULL_FILE_PLACEHOLDER, new ChainedMapper())
            .isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper13() {
    // Arrange
    Copy copy = new Copy();
    JavaConstantResource javaConstantResource = new JavaConstantResource();

    // Act and Assert
    assertTrue(copy
        .buildMap(new Resource[]{javaConstantResource, new Resource()}, Copy.NULL_FILE_PLACEHOLDER, new CutDirsMapper())
        .isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper14() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Copy copy = new Copy();
    copy.setProject(project);

    // Act and Assert
    assertTrue(
        copy.buildMap(new Resource[]{new Resource()}, Copy.NULL_FILE_PLACEHOLDER, new CutDirsMapper()).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper15() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(
        copy.buildMap(new Resource[]{new Resource(".")}, Copy.NULL_FILE_PLACEHOLDER, new FilterMapper()).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper16() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(
        copy.buildMap(new Resource[]{new Resource("..")}, Copy.NULL_FILE_PLACEHOLDER, new FilterMapper()).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper17() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(
        copy.buildMap(new Resource[]{new Resource("")}, Copy.NULL_FILE_PLACEHOLDER, new FilterMapper()).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link ClassConstants#ClassConstants()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper_givenClassConstants() {
    // Arrange
    Copy copy = new Copy();

    FilterMapper mapper = new FilterMapper();
    mapper.addClassConstants(new ClassConstants());

    // Act and Assert
    assertTrue(copy.buildMap(new Resource[]{new Resource("dirs must be set to a positive number")},
        Copy.NULL_FILE_PLACEHOLDER, mapper).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper_givenCopyProjectIsProject() {
    // Arrange
    Copy copy = new Copy();
    copy.setProject(new Project());

    // Act and Assert
    assertTrue(
        copy.buildMap(new Resource[]{new Resource()}, Copy.NULL_FILE_PLACEHOLDER, new CutDirsMapper()).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper_givenCopy_whenNull_thenReturnEmpty() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(copy.buildMap(new Resource[]{new Resource()}, Copy.NULL_FILE_PLACEHOLDER, null).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link ExpandProperties#ExpandProperties()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper_givenExpandProperties() {
    // Arrange
    Copy copy = new Copy();

    FilterMapper mapper = new FilterMapper();
    mapper.addExpandProperties(new ExpandProperties());

    // Act and Assert
    assertTrue(copy.buildMap(new Resource[]{new Resource("dirs must be set to a positive number")},
        Copy.NULL_FILE_PLACEHOLDER, mapper).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link HeadFilter#HeadFilter()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper_givenHeadFilter() {
    // Arrange
    Copy copy = new Copy();

    FilterMapper mapper = new FilterMapper();
    mapper.addHeadFilter(new HeadFilter());

    // Act and Assert
    assertTrue(copy.buildMap(new Resource[]{new Resource("dirs must be set to a positive number")},
        Copy.NULL_FILE_PLACEHOLDER, mapper).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <ul>
   *   <li>Given {@link TokenFilter#TokenFilter()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper_givenTokenFilter() {
    // Arrange
    Copy copy = new Copy();

    FilterMapper mapper = new FilterMapper();
    mapper.addTokenFilter(new TokenFilter());

    // Act and Assert
    assertTrue(copy.buildMap(new Resource[]{new Resource("dirs must be set to a positive number")},
        Copy.NULL_FILE_PLACEHOLDER, mapper).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <ul>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper_thenReturnSizeIsOne() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertEquals(1,
        copy.buildMap(
            new Resource[]{new Resource("dirs must be set to a positive number", true, WaitFor.ONE_MILLISECOND)},
            Copy.NULL_FILE_PLACEHOLDER, new FilterMapper()).size());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <ul>
   *   <li>When array of {@link Resource} with {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper_whenArrayOfResourceWithNull() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(copy.buildMap(new Resource[]{null}, Copy.NULL_FILE_PLACEHOLDER, new CutDirsMapper()).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <ul>
   *   <li>When array of {@link Resource} with {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper_whenArrayOfResourceWithResource() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(
        copy.buildMap(new Resource[]{new Resource()}, Copy.NULL_FILE_PLACEHOLDER, new CutDirsMapper()).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <ul>
   *   <li>When {@link CompositeMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper_whenCompositeMapper() {
    // Arrange
    Copy copy = new Copy();
    copy.setOverwrite(true);

    // Act and Assert
    assertTrue(
        copy.buildMap(new Resource[]{new Resource()}, Copy.NULL_FILE_PLACEHOLDER, new CompositeMapper()).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <ul>
   *   <li>When empty array of {@link Resource}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper_whenEmptyArrayOfResource() {
    // Arrange
    Copy copy = new Copy();
    copy.setOverwrite(true);

    // Act and Assert
    assertTrue(copy.buildMap(new Resource[]{}, Copy.NULL_FILE_PLACEHOLDER, new CutDirsMapper()).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <ul>
   *   <li>When {@link FilterMapper} (default constructor).</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper_whenFilterMapper_thenReturnEmpty() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(copy.buildMap(new Resource[]{new Resource()}, Copy.NULL_FILE_PLACEHOLDER, new FilterMapper()).isEmpty());
  }

  /**
   * Test {@link Copy#buildMap(Resource[], File, FileNameMapper)} with {@code fromResources}, {@code toDir}, {@code mapper}.
   * <ul>
   *   <li>When {@link ScriptMapper} (default constructor).</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#buildMap(Resource[], File, FileNameMapper)}
   */
  @Test
  public void testBuildMapWithFromResourcesToDirMapper_whenScriptMapper_thenReturnEmpty() {
    // Arrange
    Copy copy = new Copy();

    // Act and Assert
    assertTrue(copy.buildMap(new Resource[]{new Resource()}, Copy.NULL_FILE_PLACEHOLDER, new ScriptMapper()).isEmpty());
  }

  /**
   * Test {@link Copy#supportsNonFileResources()}.
   * <ul>
   *   <li>Given {@link Copy} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#supportsNonFileResources()}
   */
  @Test
  public void testSupportsNonFileResources_givenCopy_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new Copy()).supportsNonFileResources());
  }

  /**
   * Test {@link Copy#supportsNonFileResources()}.
   * <ul>
   *   <li>Given {@link Move} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Copy#supportsNonFileResources()}
   */
  @Test
  public void testSupportsNonFileResources_givenMove_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new Move()).supportsNonFileResources());
  }
}
