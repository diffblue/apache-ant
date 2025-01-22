package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.List;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.filters.ClassConstants;
import org.apache.tools.ant.filters.ExpandProperties;
import org.apache.tools.ant.filters.HeadFilter;
import org.apache.tools.ant.filters.TokenFilter;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Mapper;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.mappers.FilterMapper;
import org.apache.tools.ant.types.optional.ScriptMapper;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.apache.tools.ant.types.resources.Union;
import org.apache.tools.ant.util.CompositeMapper;
import org.apache.tools.ant.util.FileNameMapper;
import org.junit.Test;

public class UpToDateDiffblueTest {
  /**
   * Test {@link UpToDate#createSrcResources()}.
   * <p>
   * Method under test: {@link UpToDate#createSrcResources()}
   */
  @Test
  public void testCreateSrcResources() {
    // Arrange and Act
    Union actualCreateSrcResourcesResult = (new UpToDate()).createSrcResources();

    // Assert
    Location location = actualCreateSrcResourcesResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateSrcResourcesResult.getDescription());
    assertNull(actualCreateSrcResourcesResult.getProject());
    assertNull(actualCreateSrcResourcesResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateSrcResourcesResult.size());
    assertFalse(actualCreateSrcResourcesResult.isReference());
    assertTrue(actualCreateSrcResourcesResult.getResourceCollections().isEmpty());
    assertTrue(actualCreateSrcResourcesResult.isEmpty());
    assertTrue(actualCreateSrcResourcesResult.isCache());
  }

  /**
   * Test {@link UpToDate#createMapper()}.
   * <ul>
   *   <li>Given {@link UpToDate} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#createMapper()}
   */
  @Test
  public void testCreateMapper_givenUpToDateAddCutDirsMapper_thenThrowBuildException() throws BuildException {
    // Arrange
    UpToDate upToDate = new UpToDate();
    upToDate.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> upToDate.createMapper());
  }

  /**
   * Test {@link UpToDate#createMapper()}.
   * <ul>
   *   <li>Given {@link UpToDate} (default constructor).</li>
   *   <li>Then {@link UpToDate} (default constructor) {@link UpToDate#mapperElement} Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#createMapper()}
   */
  @Test
  public void testCreateMapper_givenUpToDate_thenUpToDateMapperElementDescriptionIsNull() throws BuildException {
    // Arrange
    UpToDate upToDate = new UpToDate();

    // Act
    Mapper actualCreateMapperResult = upToDate.createMapper();

    // Assert
    Mapper mapper = upToDate.mapperElement;
    assertNull(mapper.getDescription());
    assertNull(mapper.getProject());
    assertNull(mapper.getRefid());
    assertFalse(mapper.isReference());
    assertSame(upToDate.mapperElement, actualCreateMapperResult);
  }

  /**
   * Test {@link UpToDate#add(FileNameMapper)}.
   * <ul>
   *   <li>Given {@link UpToDate} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#add(FileNameMapper)}
   */
  @Test
  public void testAdd_givenUpToDateAddCutDirsMapper_thenThrowBuildException() {
    // Arrange
    UpToDate upToDate = new UpToDate();
    upToDate.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> upToDate.add(new CutDirsMapper()));
  }

  /**
   * Test {@link UpToDate#add(FileNameMapper)}.
   * <ul>
   *   <li>Given {@link UpToDate} (default constructor).</li>
   *   <li>Then {@link UpToDate} (default constructor) {@link UpToDate#mapperElement} Implementation {@link CompositeMapper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#add(FileNameMapper)}
   */
  @Test
  public void testAdd_givenUpToDate_thenUpToDateMapperElementImplementationCompositeMapper() throws BuildException {
    // Arrange
    UpToDate upToDate = new UpToDate();
    CutDirsMapper fileNameMapper = new CutDirsMapper();

    // Act
    upToDate.add(fileNameMapper);

    // Assert
    Mapper mapper = upToDate.mapperElement;
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
   * Test {@link UpToDate#eval()}.
   * <ul>
   *   <li>Given {@link UpToDate} (default constructor) addSrcfiles {@link FileSet#FileSet()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#eval()}
   */
  @Test
  public void testEval_givenUpToDateAddSrcfilesFileSet_thenThrowBuildException() {
    // Arrange
    UpToDate upToDate = new UpToDate();
    upToDate.addSrcfiles(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> upToDate.eval());
  }

  /**
   * Test {@link UpToDate#eval()}.
   * <ul>
   *   <li>Given {@link UpToDate} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#eval()}
   */
  @Test
  public void testEval_givenUpToDateProjectIsProject_thenReturnTrue() {
    // Arrange
    FileSet fs = new FileSet();
    fs.setFile(Copy.NULL_FILE_PLACEHOLDER);
    fs.appendSelector(new ScriptSelector());

    UpToDate upToDate = new UpToDate();
    upToDate.setProject(new Project());
    upToDate.add(new CutDirsMapper());
    upToDate.addSrcfiles(fs);

    // Act and Assert
    assertTrue(upToDate.eval());
  }

  /**
   * Test {@link UpToDate#eval()}.
   * <ul>
   *   <li>Given {@link UpToDate} (default constructor) Srcfile is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#eval()}
   */
  @Test
  public void testEval_givenUpToDateSrcfileIsNull_file_placeholder_thenThrowBuildException() {
    // Arrange
    UpToDate upToDate = new UpToDate();
    upToDate.setSrcfile(Copy.NULL_FILE_PLACEHOLDER);
    upToDate.addSrcfiles(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> upToDate.eval());
  }

  /**
   * Test {@link UpToDate#eval()}.
   * <ul>
   *   <li>Given {@link UpToDate} (default constructor) Srcfile is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#eval()}
   */
  @Test
  public void testEval_givenUpToDateSrcfileIsNull_file_placeholder_thenThrowBuildException2() {
    // Arrange
    UpToDate upToDate = new UpToDate();
    upToDate.setSrcfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> upToDate.eval());
  }

  /**
   * Test {@link UpToDate#eval()}.
   * <ul>
   *   <li>Given {@link UpToDate} (default constructor) TargetFile is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#eval()}
   */
  @Test
  public void testEval_givenUpToDateTargetFileIsNull_file_placeholder_thenReturnFalse() {
    // Arrange
    UpToDate upToDate = new UpToDate();
    upToDate.setTargetFile(Copy.NULL_FILE_PLACEHOLDER);
    upToDate.addSrcfiles(new FileSet());

    // Act and Assert
    assertFalse(upToDate.eval());
  }

  /**
   * Test {@link UpToDate#eval()}.
   * <ul>
   *   <li>Given {@link UpToDate} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#eval()}
   */
  @Test
  public void testEval_givenUpToDate_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new UpToDate()).eval());
  }

  /**
   * Test {@link UpToDate#execute()}.
   * <ul>
   *   <li>Given {@link UpToDate} (default constructor) addSrcfiles {@link FileSet#FileSet()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#execute()}
   */
  @Test
  public void testExecute_givenUpToDateAddSrcfilesFileSet_thenThrowBuildException() throws BuildException {
    // Arrange
    UpToDate upToDate = new UpToDate();
    upToDate.addSrcfiles(new FileSet());
    upToDate.setProperty("property attribute is required.");

    // Act and Assert
    assertThrows(BuildException.class, () -> upToDate.execute());
  }

  /**
   * Test {@link UpToDate#execute()}.
   * <ul>
   *   <li>Given {@link UpToDate} (default constructor) Srcfile is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#execute()}
   */
  @Test
  public void testExecute_givenUpToDateSrcfileIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    UpToDate upToDate = new UpToDate();
    upToDate.setSrcfile(Copy.NULL_FILE_PLACEHOLDER);
    upToDate.addSrcfiles(new FileSet());
    upToDate.setProperty("property attribute is required.");

    // Act and Assert
    assertThrows(BuildException.class, () -> upToDate.execute());
  }

  /**
   * Test {@link UpToDate#execute()}.
   * <ul>
   *   <li>Given {@link UpToDate} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#execute()}
   */
  @Test
  public void testExecute_givenUpToDate_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new UpToDate()).execute());
  }

  /**
   * Test {@link UpToDate#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException() throws BuildException {
    // Arrange
    UpToDate upToDate = new UpToDate();
    upToDate.setProperty("property attribute is required.");

    // Act and Assert
    assertThrows(BuildException.class, () -> upToDate.execute());
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir() {
    // Arrange
    UpToDate upToDate = new UpToDate();
    upToDate.setTargetFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertTrue(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{".."}));
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <ul>
   *   <li>Given {@link FilterMapper} (default constructor) addClassConstants {@link ClassConstants#ClassConstants()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir_givenFilterMapperAddClassConstantsClassConstants() {
    // Arrange
    FilterMapper fileNameMapper = new FilterMapper();
    fileNameMapper.addClassConstants(new ClassConstants());

    UpToDate upToDate = new UpToDate();
    upToDate.add(fileNameMapper);

    // Act and Assert
    assertTrue(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}));
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <ul>
   *   <li>Given {@link FilterMapper} (default constructor) addClassConstants {@link ClassConstants#ClassConstants()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir_givenFilterMapperAddClassConstantsClassConstants2() {
    // Arrange
    FilterMapper fileNameMapper = new FilterMapper();
    fileNameMapper.addClassConstants(new ClassConstants());
    fileNameMapper.addHeadFilter(new HeadFilter());

    UpToDate upToDate = new UpToDate();
    upToDate.add(fileNameMapper);

    // Act and Assert
    assertTrue(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}));
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <ul>
   *   <li>Given {@link FilterMapper} (default constructor) addExpandProperties {@link ExpandProperties#ExpandProperties()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir_givenFilterMapperAddExpandPropertiesExpandProperties() {
    // Arrange
    FilterMapper fileNameMapper = new FilterMapper();
    fileNameMapper.addExpandProperties(new ExpandProperties());

    UpToDate upToDate = new UpToDate();
    upToDate.add(fileNameMapper);

    // Act and Assert
    assertTrue(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}));
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <ul>
   *   <li>Given {@link FilterMapper} (default constructor) addExpandProperties {@link ExpandProperties#ExpandProperties()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir_givenFilterMapperAddExpandPropertiesExpandProperties2() {
    // Arrange
    FilterMapper fileNameMapper = new FilterMapper();
    fileNameMapper.addExpandProperties(new ExpandProperties());
    fileNameMapper.addHeadFilter(new HeadFilter());

    UpToDate upToDate = new UpToDate();
    upToDate.add(fileNameMapper);

    // Act and Assert
    assertTrue(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}));
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <ul>
   *   <li>Given {@link FilterMapper} (default constructor) addHeadFilter {@link HeadFilter#HeadFilter()}.</li>
   *   <li>When array of {@link String} with {@code Files}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir_givenFilterMapperAddHeadFilterHeadFilter_whenArrayOfStringWithFiles() {
    // Arrange
    FilterMapper fileNameMapper = new FilterMapper();
    fileNameMapper.addHeadFilter(new HeadFilter());

    UpToDate upToDate = new UpToDate();
    upToDate.add(fileNameMapper);

    // Act and Assert
    assertTrue(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}));
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <ul>
   *   <li>Given {@link FilterMapper} (default constructor) addHeadFilter {@link HeadFilter#HeadFilter()}.</li>
   *   <li>When array of {@link String} with {@code Files}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir_givenFilterMapperAddHeadFilterHeadFilter_whenArrayOfStringWithFiles2() {
    // Arrange
    FilterMapper fileNameMapper = new FilterMapper();
    fileNameMapper.addHeadFilter(new HeadFilter());
    fileNameMapper.addHeadFilter(new HeadFilter());

    UpToDate upToDate = new UpToDate();
    upToDate.add(fileNameMapper);

    // Act and Assert
    assertTrue(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}));
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <ul>
   *   <li>Given {@link FilterMapper} (default constructor) addTokenFilter {@link TokenFilter#TokenFilter()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir_givenFilterMapperAddTokenFilterTokenFilter() {
    // Arrange
    FilterMapper fileNameMapper = new FilterMapper();
    fileNameMapper.addTokenFilter(new TokenFilter());

    UpToDate upToDate = new UpToDate();
    upToDate.add(fileNameMapper);

    // Act and Assert
    assertTrue(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}));
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <ul>
   *   <li>Given {@link FilterMapper} (default constructor) addTokenFilter {@link TokenFilter#TokenFilter()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir_givenFilterMapperAddTokenFilterTokenFilter2() {
    // Arrange
    FilterMapper fileNameMapper = new FilterMapper();
    fileNameMapper.addTokenFilter(new TokenFilter());
    fileNameMapper.addHeadFilter(new HeadFilter());

    UpToDate upToDate = new UpToDate();
    upToDate.add(fileNameMapper);

    // Act and Assert
    assertTrue(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}));
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <ul>
   *   <li>Given {@link FilterMapper} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>When array of {@link String} with {@code Files}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir_givenFilterMapperProjectIsProject_whenArrayOfStringWithFiles() {
    // Arrange
    FilterMapper fileNameMapper = new FilterMapper();
    fileNameMapper.setProject(new Project());
    fileNameMapper.addExpandProperties(new ExpandProperties());

    UpToDate upToDate = new UpToDate();
    upToDate.add(fileNameMapper);

    // Act and Assert
    assertTrue(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}));
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    UpToDate upToDate = new UpToDate();
    upToDate.setProject(project);
    upToDate.add(new CutDirsMapper());

    // Act and Assert
    assertTrue(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}));
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   *   <li>When array of {@link String} with {@code Files}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir_givenProjectAddBuildListenerDefaultLogger_whenArrayOfStringWithFiles() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    UpToDate upToDate = new UpToDate();
    upToDate.setProject(project);
    upToDate.add(new CutDirsMapper());

    // Act and Assert
    assertTrue(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}));
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <ul>
   *   <li>Given {@link UpToDate} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>When array of {@link String} with {@code ..}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir_givenUpToDateAddCutDirsMapper_whenArrayOfStringWithDotDot() {
    // Arrange
    UpToDate upToDate = new UpToDate();
    upToDate.add(new CutDirsMapper());

    // Act and Assert
    assertTrue(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{".."}));
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <ul>
   *   <li>Given {@link UpToDate} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>When array of {@link String} with {@code Files}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir_givenUpToDateAddCutDirsMapper_whenArrayOfStringWithFiles() {
    // Arrange
    UpToDate upToDate = new UpToDate();
    upToDate.add(new CutDirsMapper());

    // Act and Assert
    assertTrue(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}));
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <ul>
   *   <li>Given {@link UpToDate} (default constructor) add {@link FilterMapper} (default constructor).</li>
   *   <li>When array of {@link String} with empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir_givenUpToDateAddFilterMapper_whenArrayOfStringWithEmptyString() {
    // Arrange
    UpToDate upToDate = new UpToDate();
    upToDate.add(new FilterMapper());

    // Act and Assert
    assertTrue(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{""}));
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <ul>
   *   <li>Given {@link UpToDate} (default constructor) add {@link FilterMapper} (default constructor).</li>
   *   <li>When array of {@link String} with {@code Files}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir_givenUpToDateAddFilterMapper_whenArrayOfStringWithFiles() {
    // Arrange
    UpToDate upToDate = new UpToDate();
    upToDate.add(new FilterMapper());

    // Act and Assert
    assertTrue(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}));
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <ul>
   *   <li>Given {@link UpToDate} (default constructor) add {@link ScriptMapper} (default constructor).</li>
   *   <li>When array of {@link String} with {@code Files}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir_givenUpToDateAddScriptMapper_whenArrayOfStringWithFiles() {
    // Arrange
    UpToDate upToDate = new UpToDate();
    upToDate.add(new ScriptMapper());

    // Act and Assert
    assertTrue(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}));
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <ul>
   *   <li>Given {@link UpToDate} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>When array of {@link String} with {@code Files}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir_givenUpToDateProjectIsProject_whenArrayOfStringWithFiles() {
    // Arrange
    UpToDate upToDate = new UpToDate();
    upToDate.setProject(new Project());
    upToDate.add(new CutDirsMapper());

    // Act and Assert
    assertTrue(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}));
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <ul>
   *   <li>Given {@link UpToDate} (default constructor) TargetFile is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir_givenUpToDateTargetFileIsNull_file_placeholder() {
    // Arrange
    UpToDate upToDate = new UpToDate();
    upToDate.setTargetFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertTrue(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"Files"}));
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <ul>
   *   <li>When array of {@link String} with {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir_whenArrayOfStringWithDot() {
    // Arrange
    UpToDate upToDate = new UpToDate();
    upToDate.setTargetFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertTrue(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{"."}));
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <ul>
   *   <li>When array of {@link String} with {@code ..}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir_whenArrayOfStringWithDotDot_thenReturnFalse() {
    // Arrange
    UpToDate upToDate = new UpToDate();
    upToDate.setTargetFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertFalse(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{".."}));
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <ul>
   *   <li>When array of {@link String} with empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir_whenArrayOfStringWithEmptyString() {
    // Arrange
    UpToDate upToDate = new UpToDate();
    upToDate.setTargetFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertTrue(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{""}));
  }

  /**
   * Test {@link UpToDate#scanDir(File, String[])}.
   * <ul>
   *   <li>When empty array of {@link String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UpToDate#scanDir(File, String[])}
   */
  @Test
  public void testScanDir_whenEmptyArrayOfString() {
    // Arrange
    UpToDate upToDate = new UpToDate();
    upToDate.setTargetFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertTrue(upToDate.scanDir(Copy.NULL_FILE_PLACEHOLDER, new String[]{}));
  }

  /**
   * Test new {@link UpToDate} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link UpToDate}
   */
  @Test
  public void testNewUpToDate() {
    // Arrange and Act
    UpToDate actualUpToDate = new UpToDate();

    // Assert
    Location location = actualUpToDate.getLocation();
    assertNull(location.getFileName());
    assertNull(actualUpToDate.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualUpToDate.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualUpToDate.getTaskName());
    assertNull(actualUpToDate.getTaskType());
    assertNull(actualUpToDate.getProject());
    assertNull(actualUpToDate.getOwningTarget());
    assertNull(actualUpToDate.mapperElement);
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualUpToDate, runtimeConfigurableWrapper.getProxy());
  }
}
