package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.taskdefs.Replace.NestedString;
import org.apache.tools.ant.taskdefs.Replace.Replacefilter;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.JavaConstantResource;
import org.apache.tools.ant.types.resources.MappedResource;
import org.junit.Test;

public class ReplaceDiffblueTest {
  /**
   * Test {@link Replace#execute()}.
   * <p>
   * Method under test: {@link Replace#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    Replace replace = new Replace();
    replace.setReplaceFilterResource(new FileResource(Copy.NULL_FILE_PLACEHOLDER));

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.execute());
  }

  /**
   * Test {@link Replace#execute()}.
   * <p>
   * Method under test: {@link Replace#execute()}
   */
  @Test
  public void testExecute2() throws BuildException {
    // Arrange
    Replace replace = new Replace();
    replace.setReplaceFilterResource(new FileResource(Copy.NULL_FILE_PLACEHOLDER, Manifest.ATTRIBUTE_NAME));

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.execute());
  }

  /**
   * Test {@link Replace#execute()}.
   * <p>
   * Method under test: {@link Replace#execute()}
   */
  @Test
  public void testExecute3() throws BuildException {
    // Arrange
    Replace replace = new Replace();
    replace.setReplaceFilterResource(
        new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.execute());
  }

  /**
   * Test {@link Replace#execute()}.
   * <p>
   * Method under test: {@link Replace#execute()}
   */
  @Test
  public void testExecute4() throws BuildException {
    // Arrange
    Replace replace = new Replace();
    replace.setReplaceFilterResource(new FileResource((File) null, Manifest.ATTRIBUTE_NAME));

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.execute());
  }

  /**
   * Test {@link Replace#execute()}.
   * <p>
   * Method under test: {@link Replace#execute()}
   */
  @Test
  public void testExecute5() throws BuildException {
    // Arrange
    Replace replace = new Replace();
    replace.setReplaceFilterResource(new FileResource(Copy.NULL_FILE_PLACEHOLDER, "."));

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.execute());
  }

  /**
   * Test {@link Replace#execute()}.
   * <p>
   * Method under test: {@link Replace#execute()}
   */
  @Test
  public void testExecute6() throws BuildException {
    // Arrange
    Replace replace = new Replace();
    replace.setReplaceFilterResource(new FileResource(Copy.NULL_FILE_PLACEHOLDER, ".."));

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.execute());
  }

  /**
   * Test {@link Replace#execute()}.
   * <p>
   * Method under test: {@link Replace#execute()}
   */
  @Test
  public void testExecute7() throws BuildException {
    // Arrange
    Replace replace = new Replace();
    replace.setReplaceFilterResource(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile()));

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.execute());
  }

  /**
   * Test {@link Replace#execute()}.
   * <p>
   * Method under test: {@link Replace#execute()}
   */
  @Test
  public void testExecute8() throws BuildException {
    // Arrange
    Replace replace = new Replace();
    replace.setReplaceFilterResource(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile()));

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.execute());
  }

  /**
   * Test {@link Replace#execute()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code file attribute is null!}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Replace#execute()}
   */
  @Test
  public void testExecute_givenFileResourceNameIsFileAttributeIsNull_thenThrowBuildException() throws BuildException {
    // Arrange
    FileResource replaceFilter = new FileResource();
    replaceFilter.setName("file attribute is null!");

    Replace replace = new Replace();
    replace.setReplaceFilterResource(replaceFilter);

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.execute());
  }

  /**
   * Test {@link Replace#execute()}.
   * <ul>
   *   <li>Given {@link Replace} (default constructor) Dir is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Replace#execute()}
   */
  @Test
  public void testExecute_givenReplaceDirIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    Replace replace = new Replace();
    replace.setDir(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.execute());
  }

  /**
   * Test {@link Replace#execute()}.
   * <ul>
   *   <li>Given {@link Replace} (default constructor) File is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Replace#execute()}
   */
  @Test
  public void testExecute_givenReplaceFileIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    Replace replace = new Replace();
    replace.setFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.execute());
  }

  /**
   * Test {@link Replace#execute()}.
   * <ul>
   *   <li>Given {@link Replace} (default constructor) ReplaceFilterResource is {@link JavaConstantResource} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Replace#execute()}
   */
  @Test
  public void testExecute_givenReplaceReplaceFilterResourceIsJavaConstantResource() throws BuildException {
    // Arrange
    Replace replace = new Replace();
    replace.setReplaceFilterResource(new JavaConstantResource());

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.execute());
  }

  /**
   * Test {@link Replace#execute()}.
   * <ul>
   *   <li>Given {@link Replace} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Replace#execute()}
   */
  @Test
  public void testExecute_givenReplace_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Replace()).execute());
  }

  /**
   * Test NestedString {@link NestedString#addText(String)}.
   * <p>
   * Method under test: {@link NestedString#addText(String)}
   */
  @Test
  public void testNestedStringAddText() {
    // Arrange
    NestedString nestedString = (new Replace()).new NestedString();

    // Act
    nestedString.addText("Val");

    // Assert
    assertEquals("Val", nestedString.getText());
  }

  /**
   * Test NestedString {@link NestedString#getText()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link NestedString#getText()}
   */
  @Test
  public void testNestedStringGetText_givenJavaLangObject_thenReturnEmptyString() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Adding reference: ", typeClass);
    project.addBuildListener(new AntClassLoader());

    Replace replace = new Replace();
    replace.setProject(project);

    NestedString nestedString = replace.new NestedString();
    nestedString.setExpandProperties(true);

    // Act and Assert
    assertEquals("", nestedString.getText());
  }

  /**
   * Test NestedString {@link NestedString#getText()}.
   * <ul>
   *   <li>Given {@link NestedString#NestedString(Replace)} with this$0 is {@link Replace} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link NestedString#getText()}
   */
  @Test
  public void testNestedStringGetText_givenNestedStringWithThis$0IsReplace() {
    // Arrange, Act and Assert
    assertEquals("", ((new Replace()).new NestedString()).getText());
  }

  /**
   * Test NestedString {@link NestedString#getText()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NestedString#getText()}
   */
  @Test
  public void testNestedStringGetText_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Replace replace = new Replace();
    replace.setProject(project);

    NestedString nestedString = replace.new NestedString();
    nestedString.setExpandProperties(true);

    // Act and Assert
    assertEquals("", nestedString.getText());
  }

  /**
   * Test NestedString {@link NestedString#getText()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NestedString#getText()}
   */
  @Test
  public void testNestedStringGetText_givenProjectAddTargetAddingReferenceAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    Replace replace = new Replace();
    replace.setProject(project);

    NestedString nestedString = replace.new NestedString();
    nestedString.setExpandProperties(true);

    // Act and Assert
    assertEquals("", nestedString.getText());
  }

  /**
   * Test NestedString {@link NestedString#getText()}.
   * <ul>
   *   <li>Given {@link Replace} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link NestedString#getText()}
   */
  @Test
  public void testNestedStringGetText_givenReplaceProjectIsProject_thenReturnEmptyString() {
    // Arrange
    Replace replace = new Replace();
    replace.setProject(new Project());

    NestedString nestedString = replace.new NestedString();
    nestedString.setExpandProperties(true);

    // Act and Assert
    assertEquals("", nestedString.getText());
  }

  /**
   * Test Replacefilter {@link Replacefilter#createReplaceToken()}.
   * <p>
   * Method under test: {@link Replacefilter#createReplaceToken()}
   */
  @Test
  public void testReplacefilterCreateReplaceToken() {
    // Arrange
    Replacefilter replacefilter = (new Replace()).new Replacefilter();

    // Act and Assert
    assertEquals("", replacefilter.createReplaceToken().getText());
    assertEquals("", replacefilter.getToken());
  }

  /**
   * Test Replacefilter {@link Replacefilter#createReplaceValue()}.
   * <p>
   * Method under test: {@link Replacefilter#createReplaceValue()}
   */
  @Test
  public void testReplacefilterCreateReplaceValue() {
    // Arrange
    Replacefilter replacefilter = (new Replace()).new Replacefilter();

    // Act and Assert
    assertEquals("", replacefilter.createReplaceValue().getText());
    assertEquals("", replacefilter.getValue());
  }

  /**
   * Test Replacefilter {@link Replacefilter#getReplaceValue()}.
   * <ul>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Replacefilter#getReplaceValue()}
   */
  @Test
  public void testReplacefilterGetReplaceValue_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", ((new Replace()).new Replacefilter()).getReplaceValue());
  }

  /**
   * Test Replacefilter getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Replacefilter#Replacefilter(Replace)}
   *   <li>{@link Replacefilter#setInputBuffer(StringBuffer)}
   *   <li>{@link Replacefilter#setProperty(String)}
   *   <li>{@link Replacefilter#getOutputBuffer()}
   *   <li>{@link Replacefilter#getProperty()}
   * </ul>
   */
  @Test
  public void testReplacefilterGettersAndSetters() {
    // Arrange and Act
    Replacefilter actualReplacefilter = (new Replace()).new Replacefilter();
    actualReplacefilter.setInputBuffer(new StringBuffer("foo"));
    actualReplacefilter.setProperty("Property");
    StringBuffer actualOutputBuffer = actualReplacefilter.getOutputBuffer();
    String actualProperty = actualReplacefilter.getProperty();

    // Assert
    assertEquals("", actualOutputBuffer.toString());
    assertEquals("Property", actualProperty);
  }

  /**
   * Test Replacefilter {@link Replacefilter#setToken(String)}.
   * <p>
   * Method under test: {@link Replacefilter#setToken(String)}
   */
  @Test
  public void testReplacefilterSetToken() {
    // Arrange
    Replacefilter replacefilter = (new Replace()).new Replacefilter();

    // Act
    replacefilter.setToken("foo");

    // Assert
    assertEquals("foo", replacefilter.getToken());
  }

  /**
   * Test Replacefilter {@link Replacefilter#setValue(String)}.
   * <p>
   * Method under test: {@link Replacefilter#setValue(String)}
   */
  @Test
  public void testReplacefilterSetValue() {
    // Arrange
    Replacefilter replacefilter = (new Replace()).new Replacefilter();

    // Act
    replacefilter.setValue("42");

    // Assert
    assertEquals("42", replacefilter.getReplaceValue());
    assertEquals("42", replacefilter.getValue());
  }

  /**
   * Test Replacefilter {@link Replacefilter#validate()}.
   * <p>
   * Method under test: {@link Replacefilter#validate()}
   */
  @Test
  public void testReplacefilterValidate() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ((new Replace()).new Replacefilter()).validate());
  }

  /**
   * Test {@link Replace#validateAttributes()}.
   * <p>
   * Method under test: {@link Replace#validateAttributes()}
   */
  @Test
  public void testValidateAttributes() throws BuildException {
    // Arrange
    Replace replace = new Replace();
    replace.setPropertyResource(new Resource("Either token or a nested replacefilter must be specified"));
    replace.setFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.validateAttributes());
  }

  /**
   * Test {@link Replace#validateAttributes()}.
   * <p>
   * Method under test: {@link Replace#validateAttributes()}
   */
  @Test
  public void testValidateAttributes2() throws BuildException {
    // Arrange
    Replace replace = new Replace();
    replace.setPropertyResource(
        new Resource("Either token or a nested replacefilter must be specified", true, WaitFor.ONE_MILLISECOND));
    replace.setFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.validateAttributes());
  }

  /**
   * Test {@link Replace#validateAttributes()}.
   * <p>
   * Method under test: {@link Replace#validateAttributes()}
   */
  @Test
  public void testValidateAttributes3() throws BuildException {
    // Arrange
    Replace replace = new Replace();
    replace.setPropertyResource(new FileResource(Copy.NULL_FILE_PLACEHOLDER));
    replace.setFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.validateAttributes());
  }

  /**
   * Test {@link Replace#validateAttributes()}.
   * <p>
   * Method under test: {@link Replace#validateAttributes()}
   */
  @Test
  public void testValidateAttributes4() throws BuildException {
    // Arrange
    Replace replace = new Replace();
    replace.setPropertyResource(
        new FileResource(Copy.NULL_FILE_PLACEHOLDER, "Either token or a nested replacefilter must be specified"));
    replace.setFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.validateAttributes());
  }

  /**
   * Test {@link Replace#validateAttributes()}.
   * <p>
   * Method under test: {@link Replace#validateAttributes()}
   */
  @Test
  public void testValidateAttributes5() throws BuildException {
    // Arrange
    Replace replace = new Replace();
    Resource r = new Resource();
    replace.setPropertyResource(new MappedResource(r, new CutDirsMapper()));
    replace.setFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.validateAttributes());
  }

  /**
   * Test {@link Replace#validateAttributes()}.
   * <p>
   * Method under test: {@link Replace#validateAttributes()}
   */
  @Test
  public void testValidateAttributes6() throws BuildException {
    // Arrange
    Replace replace = new Replace();
    replace
        .setPropertyResource(new FileResource((File) null, "Either token or a nested replacefilter must be specified"));
    replace.setFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.validateAttributes());
  }

  /**
   * Test {@link Replace#validateAttributes()}.
   * <p>
   * Method under test: {@link Replace#validateAttributes()}
   */
  @Test
  public void testValidateAttributes7() throws BuildException {
    // Arrange
    Replace replace = new Replace();
    replace.setPropertyResource(new FileResource(Copy.NULL_FILE_PLACEHOLDER, "."));
    replace.setFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.validateAttributes());
  }

  /**
   * Test {@link Replace#validateAttributes()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Replace#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenFileResourceNameIsFileAttributeIsNull() throws BuildException {
    // Arrange
    FileResource propertyResource = new FileResource();
    propertyResource.setName("file attribute is null!");

    Replace replace = new Replace();
    replace.setPropertyResource(propertyResource);
    replace.setFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.validateAttributes());
  }

  /**
   * Test {@link Replace#validateAttributes()}.
   * <ul>
   *   <li>Given {@link Replace} (default constructor) Dir is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Replace#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenReplaceDirIsNull_file_placeholder() throws BuildException {
    // Arrange
    Replace replace = new Replace();
    replace.setDir(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.validateAttributes());
  }

  /**
   * Test {@link Replace#validateAttributes()}.
   * <ul>
   *   <li>Given {@link Replace} (default constructor) PropertyResource is {@link JavaConstantResource} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Replace#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenReplacePropertyResourceIsJavaConstantResource() throws BuildException {
    // Arrange
    Replace replace = new Replace();
    replace.setPropertyResource(new JavaConstantResource());
    replace.setFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.validateAttributes());
  }

  /**
   * Test {@link Replace#validateAttributes()}.
   * <ul>
   *   <li>Given {@link Replace} (default constructor) PropertyResource is {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Replace#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenReplacePropertyResourceIsResource() throws BuildException {
    // Arrange
    Replace replace = new Replace();
    replace.setPropertyResource(new Resource());
    replace.setFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.validateAttributes());
  }

  /**
   * Test {@link Replace#validateAttributes()}.
   * <ul>
   *   <li>Given {@link Replace} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Replace#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_givenReplace_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Replace()).validateAttributes());
  }

  /**
   * Test {@link Replace#validateAttributes()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Replace#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_thenThrowBuildException() throws BuildException {
    // Arrange
    Replace replace = new Replace();
    replace.setFile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.validateAttributes());
  }

  /**
   * Test {@link Replace#getProperties(File)} with {@code propertyFile}.
   * <ul>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Replace#getProperties(File)}
   */
  @Test
  public void testGetPropertiesWithPropertyFile_whenNull_file_placeholder() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Replace()).getProperties(Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Replace#getProperties(File)} with {@code propertyFile}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code ..} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link Replace#getProperties(File)}
   */
  @Test
  public void testGetPropertiesWithPropertyFile_whenPropertyIsJavaIoTmpdirIsDotDotToFile() throws BuildException {
    // Arrange
    Replace replace = new Replace();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> replace.getProperties(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile()));
  }

  /**
   * Test {@link Replace#getProperties(File)} with {@code propertyFile}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code .} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link Replace#getProperties(File)}
   */
  @Test
  public void testGetPropertiesWithPropertyFile_whenPropertyIsJavaIoTmpdirIsDotToFile() throws BuildException {
    // Arrange
    Replace replace = new Replace();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> replace.getProperties(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile()));
  }

  /**
   * Test {@link Replace#getProperties(File)} with {@code propertyFile}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link Replace#getProperties(File)}
   */
  @Test
  public void testGetPropertiesWithPropertyFile_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() throws BuildException {
    // Arrange
    Replace replace = new Replace();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> replace.getProperties(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link Replace#getProperties(Resource)} with {@code propertyResource}.
   * <p>
   * Method under test: {@link Replace#getProperties(Resource)}
   */
  @Test
  public void testGetPropertiesWithPropertyResource() throws BuildException {
    // Arrange
    Replace replace = new Replace();

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.getProperties(new FileResource(Copy.NULL_FILE_PLACEHOLDER)));
  }

  /**
   * Test {@link Replace#getProperties(Resource)} with {@code propertyResource}.
   * <p>
   * Method under test: {@link Replace#getProperties(Resource)}
   */
  @Test
  public void testGetPropertiesWithPropertyResource2() throws BuildException {
    // Arrange
    Replace replace = new Replace();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> replace.getProperties(new FileResource(Copy.NULL_FILE_PLACEHOLDER, Manifest.ATTRIBUTE_NAME)));
  }

  /**
   * Test {@link Replace#getProperties(Resource)} with {@code propertyResource}.
   * <p>
   * Method under test: {@link Replace#getProperties(Resource)}
   */
  @Test
  public void testGetPropertiesWithPropertyResource3() throws BuildException {
    // Arrange
    Replace replace = new Replace();

    // Act and Assert
    assertThrows(BuildException.class, () -> replace
        .getProperties(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())));
  }

  /**
   * Test {@link Replace#getProperties(Resource)} with {@code propertyResource}.
   * <p>
   * Method under test: {@link Replace#getProperties(Resource)}
   */
  @Test
  public void testGetPropertiesWithPropertyResource4() throws BuildException {
    // Arrange
    Replace replace = new Replace();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> replace.getProperties(new FileResource((File) null, Manifest.ATTRIBUTE_NAME)));
  }

  /**
   * Test {@link Replace#getProperties(Resource)} with {@code propertyResource}.
   * <p>
   * Method under test: {@link Replace#getProperties(Resource)}
   */
  @Test
  public void testGetPropertiesWithPropertyResource5() throws BuildException {
    // Arrange
    Replace replace = new Replace();

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.getProperties(new FileResource(Copy.NULL_FILE_PLACEHOLDER, ".")));
  }

  /**
   * Test {@link Replace#getProperties(Resource)} with {@code propertyResource}.
   * <p>
   * Method under test: {@link Replace#getProperties(Resource)}
   */
  @Test
  public void testGetPropertiesWithPropertyResource6() throws BuildException {
    // Arrange
    Replace replace = new Replace();

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.getProperties(new FileResource(Copy.NULL_FILE_PLACEHOLDER, "..")));
  }

  /**
   * Test {@link Replace#getProperties(Resource)} with {@code propertyResource}.
   * <p>
   * Method under test: {@link Replace#getProperties(Resource)}
   */
  @Test
  public void testGetPropertiesWithPropertyResource7() throws BuildException {
    // Arrange
    Replace replace = new Replace();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> replace.getProperties(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile())));
  }

  /**
   * Test {@link Replace#getProperties(Resource)} with {@code propertyResource}.
   * <p>
   * Method under test: {@link Replace#getProperties(Resource)}
   */
  @Test
  public void testGetPropertiesWithPropertyResource8() throws BuildException {
    // Arrange
    Replace replace = new Replace();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> replace.getProperties(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile())));
  }

  /**
   * Test {@link Replace#getProperties(Resource)} with {@code propertyResource}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Replace#getProperties(Resource)}
   */
  @Test
  public void testGetPropertiesWithPropertyResource_givenFileAttributeIsNull() throws BuildException {
    // Arrange
    Replace replace = new Replace();

    FileResource propertyResource = new FileResource();
    propertyResource.setName("file attribute is null!");

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.getProperties(propertyResource));
  }

  /**
   * Test {@link Replace#getProperties(Resource)} with {@code propertyResource}.
   * <ul>
   *   <li>When {@link JavaConstantResource} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Replace#getProperties(Resource)}
   */
  @Test
  public void testGetPropertiesWithPropertyResource_whenJavaConstantResource() throws BuildException {
    // Arrange
    Replace replace = new Replace();

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.getProperties(new JavaConstantResource()));
  }

  /**
   * Test {@link Replace#createReplaceToken()}.
   * <p>
   * Method under test: {@link Replace#createReplaceToken()}
   */
  @Test
  public void testCreateReplaceToken() {
    // Arrange, Act and Assert
    assertEquals("", (new Replace()).createReplaceToken().getText());
  }

  /**
   * Test {@link Replace#createReplaceValue()}.
   * <p>
   * Method under test: {@link Replace#createReplaceValue()}
   */
  @Test
  public void testCreateReplaceValue() {
    // Arrange, Act and Assert
    assertEquals("", (new Replace()).createReplaceValue().getText());
  }

  /**
   * Test {@link Replace#createReplacefilter()}.
   * <p>
   * Method under test: {@link Replace#createReplacefilter()}
   */
  @Test
  public void testCreateReplacefilter() {
    // Arrange and Act
    Replacefilter actualCreateReplacefilterResult = (new Replace()).createReplacefilter();

    // Assert
    assertEquals("", actualCreateReplacefilterResult.getOutputBuffer().toString());
    assertEquals("", actualCreateReplacefilterResult.getReplaceValue());
    assertNull(actualCreateReplacefilterResult.getProperty());
  }

  /**
   * Test {@link Replace#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Replace} (default constructor).</li>
   *   <li>When {@link Concat} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Replace#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenReplace_whenConcat_thenThrowBuildException() {
    // Arrange
    Replace replace = new Replace();

    // Act and Assert
    assertThrows(BuildException.class, () -> replace.addConfigured(new Concat()));
  }

  /**
   * Test new {@link Replace} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Replace}
   */
  @Test
  public void testNewReplace() {
    // Arrange and Act
    Replace actualReplace = new Replace();

    // Assert
    assertNull(actualReplace.getDescription());
    assertNull(actualReplace.getTaskName());
    assertNull(actualReplace.getTaskType());
    assertNull(actualReplace.getProject());
    assertNull(actualReplace.getOwningTarget());
    assertFalse(actualReplace.hasSelectors());
  }
}
