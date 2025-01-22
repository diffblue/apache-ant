package org.apache.tools.ant.types;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Stack;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.util.MergingMapper;
import org.junit.Test;

public class RedirectorElementDiffblueTest {
  /**
   * Test {@link RedirectorElement#addConfiguredInputMapper(Mapper)}.
   * <ul>
   *   <li>Then not {@link RedirectorElement} (default constructor) Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link RedirectorElement#addConfiguredInputMapper(Mapper)}
   */
  @Test
  public void testAddConfiguredInputMapper_thenNotRedirectorElementChecked() {
    // Arrange
    RedirectorElement redirectorElement = new RedirectorElement();

    // Act
    redirectorElement.addConfiguredInputMapper(new Mapper(new Project()));

    // Assert
    assertFalse(redirectorElement.isChecked());
  }

  /**
   * Test {@link RedirectorElement#addConfiguredInputMapper(Mapper)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RedirectorElement#addConfiguredInputMapper(Mapper)}
   */
  @Test
  public void testAddConfiguredInputMapper_thenThrowBuildException() {
    // Arrange
    RedirectorElement redirectorElement = new RedirectorElement();
    redirectorElement.addConfiguredInputMapper(new Mapper(new Project()));

    // Act and Assert
    assertThrows(BuildException.class, () -> redirectorElement.addConfiguredInputMapper(new Mapper(new Project())));
  }

  /**
   * Test {@link RedirectorElement#addConfiguredOutputMapper(Mapper)}.
   * <ul>
   *   <li>Then not {@link RedirectorElement} (default constructor) Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link RedirectorElement#addConfiguredOutputMapper(Mapper)}
   */
  @Test
  public void testAddConfiguredOutputMapper_thenNotRedirectorElementChecked() {
    // Arrange
    RedirectorElement redirectorElement = new RedirectorElement();

    // Act
    redirectorElement.addConfiguredOutputMapper(new Mapper(new Project()));

    // Assert
    assertFalse(redirectorElement.isChecked());
  }

  /**
   * Test {@link RedirectorElement#addConfiguredOutputMapper(Mapper)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RedirectorElement#addConfiguredOutputMapper(Mapper)}
   */
  @Test
  public void testAddConfiguredOutputMapper_thenThrowBuildException() {
    // Arrange
    RedirectorElement redirectorElement = new RedirectorElement();
    redirectorElement.addConfiguredOutputMapper(new Mapper(new Project()));

    // Act and Assert
    assertThrows(BuildException.class, () -> redirectorElement.addConfiguredOutputMapper(new Mapper(new Project())));
  }

  /**
   * Test {@link RedirectorElement#addConfiguredErrorMapper(Mapper)}.
   * <ul>
   *   <li>Then not {@link RedirectorElement} (default constructor) Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link RedirectorElement#addConfiguredErrorMapper(Mapper)}
   */
  @Test
  public void testAddConfiguredErrorMapper_thenNotRedirectorElementChecked() {
    // Arrange
    RedirectorElement redirectorElement = new RedirectorElement();

    // Act
    redirectorElement.addConfiguredErrorMapper(new Mapper(new Project()));

    // Assert
    assertFalse(redirectorElement.isChecked());
  }

  /**
   * Test {@link RedirectorElement#addConfiguredErrorMapper(Mapper)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RedirectorElement#addConfiguredErrorMapper(Mapper)}
   */
  @Test
  public void testAddConfiguredErrorMapper_thenThrowBuildException() {
    // Arrange
    RedirectorElement redirectorElement = new RedirectorElement();
    redirectorElement.addConfiguredErrorMapper(new Mapper(new Project()));

    // Act and Assert
    assertThrows(BuildException.class, () -> redirectorElement.addConfiguredErrorMapper(new Mapper(new Project())));
  }

  /**
   * Test {@link RedirectorElement#setInput(File)}.
   * <ul>
   *   <li>Given {@link RedirectorElement} (default constructor) InputString is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RedirectorElement#setInput(File)}
   */
  @Test
  public void testSetInput_givenRedirectorElementInputStringIsFoo_thenThrowBuildException() {
    // Arrange
    RedirectorElement redirectorElement = new RedirectorElement();
    redirectorElement.setInputString("foo");

    // Act and Assert
    assertThrows(BuildException.class,
        () -> redirectorElement.setInput(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link RedirectorElement#setError(File)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RedirectorElement#setError(File)}
   */
  @Test
  public void testSetError_whenNull_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> (new RedirectorElement()).setError(null));
  }

  /**
   * Test {@link RedirectorElement#createInputFilterChain()}.
   * <p>
   * Method under test: {@link RedirectorElement#createInputFilterChain()}
   */
  @Test
  public void testCreateInputFilterChain() {
    // Arrange
    RedirectorElement redirectorElement = new RedirectorElement();

    // Act
    FilterChain actualCreateInputFilterChainResult = redirectorElement.createInputFilterChain();

    // Assert
    assertEquals("FilterChain", actualCreateInputFilterChainResult.getDataTypeName());
    Location location = actualCreateInputFilterChainResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateInputFilterChainResult.getDescription());
    assertNull(actualCreateInputFilterChainResult.getProject());
    assertNull(actualCreateInputFilterChainResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(redirectorElement.isChecked());
    assertFalse(actualCreateInputFilterChainResult.isReference());
    assertTrue(actualCreateInputFilterChainResult.getFilterReaders().isEmpty());
    assertTrue(actualCreateInputFilterChainResult.isChecked());
  }

  /**
   * Test {@link RedirectorElement#createOutputFilterChain()}.
   * <p>
   * Method under test: {@link RedirectorElement#createOutputFilterChain()}
   */
  @Test
  public void testCreateOutputFilterChain() {
    // Arrange
    RedirectorElement redirectorElement = new RedirectorElement();

    // Act
    FilterChain actualCreateOutputFilterChainResult = redirectorElement.createOutputFilterChain();

    // Assert
    assertEquals("FilterChain", actualCreateOutputFilterChainResult.getDataTypeName());
    Location location = actualCreateOutputFilterChainResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateOutputFilterChainResult.getDescription());
    assertNull(actualCreateOutputFilterChainResult.getProject());
    assertNull(actualCreateOutputFilterChainResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(redirectorElement.isChecked());
    assertFalse(actualCreateOutputFilterChainResult.isReference());
    assertTrue(actualCreateOutputFilterChainResult.getFilterReaders().isEmpty());
    assertTrue(actualCreateOutputFilterChainResult.isChecked());
  }

  /**
   * Test {@link RedirectorElement#createErrorFilterChain()}.
   * <p>
   * Method under test: {@link RedirectorElement#createErrorFilterChain()}
   */
  @Test
  public void testCreateErrorFilterChain() {
    // Arrange
    RedirectorElement redirectorElement = new RedirectorElement();

    // Act
    FilterChain actualCreateErrorFilterChainResult = redirectorElement.createErrorFilterChain();

    // Assert
    assertEquals("FilterChain", actualCreateErrorFilterChainResult.getDataTypeName());
    Location location = actualCreateErrorFilterChainResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateErrorFilterChainResult.getDescription());
    assertNull(actualCreateErrorFilterChainResult.getProject());
    assertNull(actualCreateErrorFilterChainResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(redirectorElement.isChecked());
    assertFalse(actualCreateErrorFilterChainResult.isReference());
    assertTrue(actualCreateErrorFilterChainResult.getFilterReaders().isEmpty());
    assertTrue(actualCreateErrorFilterChainResult.isChecked());
  }

  /**
   * Test {@link RedirectorElement#createMergeMapper(File)}.
   * <p>
   * Method under test: {@link RedirectorElement#createMergeMapper(File)}
   */
  @Test
  public void testCreateMergeMapper() throws ClassNotFoundException, BuildException {
    // Arrange
    RedirectorElement redirectorElement = new RedirectorElement();

    // Act
    Mapper actualCreateMergeMapperResult = redirectorElement
        .createMergeMapper(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    assertTrue(actualCreateMergeMapperResult.getImplementation() instanceof MergingMapper);
    assertEquals("Mapper", actualCreateMergeMapperResult.getDataTypeName());
    assertEquals("org.apache.tools.ant.util.MergingMapper", actualCreateMergeMapperResult.classname);
    assertNull(actualCreateMergeMapperResult.getDescription());
    assertNull(actualCreateMergeMapperResult.from);
    assertNull(actualCreateMergeMapperResult.getProject());
    assertNull(actualCreateMergeMapperResult.type);
    assertNull(actualCreateMergeMapperResult.classpath);
    assertNull(actualCreateMergeMapperResult.getRefid());
    assertFalse(actualCreateMergeMapperResult.isReference());
    assertTrue(actualCreateMergeMapperResult.isChecked());
    assertEquals(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(),
        actualCreateMergeMapperResult.to);
    Class<MergingMapper> expectedImplementationClass = MergingMapper.class;
    assertEquals(expectedImplementationClass, actualCreateMergeMapperResult.getImplementationClass());
  }

  /**
   * Test {@link RedirectorElement#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <p>
   * Method under test: {@link RedirectorElement#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP() throws BuildException {
    // Arrange
    RedirectorElement redirectorElement = new RedirectorElement();
    redirectorElement.addConfiguredInputMapper(new Mapper(new Project()));
    Stack<Object> stk = new Stack<>();

    // Act
    redirectorElement.dieOnCircularReference(stk, new Project());

    // Assert
    assertTrue(redirectorElement.isChecked());
  }

  /**
   * Test {@link RedirectorElement#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <p>
   * Method under test: {@link RedirectorElement#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP2() throws BuildException {
    // Arrange
    Mapper inputMapper = new Mapper(new Project());
    inputMapper.addConfigured(new CutDirsMapper());

    RedirectorElement redirectorElement = new RedirectorElement();
    redirectorElement.addConfiguredInputMapper(inputMapper);
    Stack<Object> stk = new Stack<>();

    // Act
    redirectorElement.dieOnCircularReference(stk, new Project());

    // Assert
    assertTrue(redirectorElement.isChecked());
  }

  /**
   * Test {@link RedirectorElement#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link Stack} (default constructor) add {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RedirectorElement#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_given42_whenStackAdd42() throws BuildException {
    // Arrange
    RedirectorElement redirectorElement = new RedirectorElement();

    Stack<Object> stk = new Stack<>();
    stk.add("42");

    // Act
    redirectorElement.dieOnCircularReference(stk, new Project());

    // Assert that nothing has changed
    assertTrue(redirectorElement.isChecked());
  }

  /**
   * Test {@link RedirectorElement#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link Stack} (default constructor) add {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RedirectorElement#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_given42_whenStackAdd422() throws BuildException {
    // Arrange
    RedirectorElement redirectorElement = new RedirectorElement();

    Stack<Object> stk = new Stack<>();
    stk.add("42");
    stk.add("42");

    // Act
    redirectorElement.dieOnCircularReference(stk, new Project());

    // Assert that nothing has changed
    assertTrue(redirectorElement.isChecked());
  }

  /**
   * Test {@link RedirectorElement#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@link RedirectorElement} (default constructor).</li>
   *   <li>When {@link Stack} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link RedirectorElement#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_givenRedirectorElement_whenStack() throws BuildException {
    // Arrange
    RedirectorElement redirectorElement = new RedirectorElement();
    Stack<Object> stk = new Stack<>();

    // Act
    redirectorElement.dieOnCircularReference(stk, new Project());

    // Assert that nothing has changed
    assertTrue(redirectorElement.isChecked());
  }

  /**
   * Test new {@link RedirectorElement} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link RedirectorElement}
   */
  @Test
  public void testNewRedirectorElement() {
    // Arrange and Act
    RedirectorElement actualRedirectorElement = new RedirectorElement();

    // Assert
    assertEquals("RedirectorElement", actualRedirectorElement.getDataTypeName());
    Location location = actualRedirectorElement.getLocation();
    assertNull(location.getFileName());
    assertNull(actualRedirectorElement.getDescription());
    assertNull(actualRedirectorElement.getProject());
    assertNull(actualRedirectorElement.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualRedirectorElement.isReference());
    assertTrue(actualRedirectorElement.isChecked());
  }
}
