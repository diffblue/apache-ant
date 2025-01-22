package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class ReflectUtilDiffblueTest {
  /**
   * Test {@link ReflectUtil#newInstance(Class, Class[], Object[])}.
   * <ul>
   *   <li>When array of {@link Class} with {@link Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReflectUtil#newInstance(Class, Class[], Object[])}
   */
  @Test
  public void testNewInstance_whenArrayOfClassWithObject_thenThrowBuildException() {
    // Arrange
    Class<Object> ofClass = Object.class;
    Class<Object> forNameResult = Object.class;

    // Act and Assert
    assertThrows(BuildException.class,
        () -> ReflectUtil.newInstance(ofClass, new Class[]{forNameResult}, new Object[]{"Args"}));
  }

  /**
   * Test {@link ReflectUtil#newInstance(Class, Class[], Object[])}.
   * <ul>
   *   <li>When array of {@link Object} with {@code Args}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReflectUtil#newInstance(Class, Class[], Object[])}
   */
  @Test
  public void testNewInstance_whenArrayOfObjectWithArgs_thenThrowBuildException() {
    // Arrange
    Class<Object> ofClass = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> ReflectUtil.newInstance(ofClass, new Class[]{}, new Object[]{"Args"}));
  }

  /**
   * Test {@link ReflectUtil#newInstance(Class, Class[], Object[])}.
   * <ul>
   *   <li>When {@code BuildException}.</li>
   *   <li>Then return {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReflectUtil#newInstance(Class, Class[], Object[])}
   */
  @Test
  public void testNewInstance_whenOrgApacheToolsAntBuildException_thenReturnBuildException() {
    // Arrange
    Class<BuildException> ofClass = BuildException.class;

    // Act
    Object actualNewInstanceResult = ReflectUtil.newInstance(ofClass, new Class[]{}, new Object[]{});

    // Assert
    assertTrue(actualNewInstanceResult instanceof BuildException);
    assertNull(((BuildException) actualNewInstanceResult).getLocalizedMessage());
    assertNull(((BuildException) actualNewInstanceResult).getMessage());
    Location location = ((BuildException) actualNewInstanceResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((BuildException) actualNewInstanceResult).getCause());
    assertNull(((BuildException) actualNewInstanceResult).getException());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, ((BuildException) actualNewInstanceResult).getSuppressed().length);
  }

  /**
   * Test {@link ReflectUtil#invoke(Object, String, Class, Object, Class, Object)} with {@code obj}, {@code methodName}, {@code argType1}, {@code arg1}, {@code argType2}, {@code arg2}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReflectUtil#invoke(Object, String, Class, Object, Class, Object)}
   */
  @Test
  public void testInvokeWithObjMethodNameArgType1Arg1ArgType2Arg2_thenThrowBuildException() {
    // Arrange
    Class<Object> argType1 = Object.class;
    Class<Object> argType2 = Object.class;

    // Act and Assert
    assertThrows(BuildException.class,
        () -> ReflectUtil.invoke("Obj", "Method Name", argType1, "Arg1", argType2, "Arg2"));
  }

  /**
   * Test {@link ReflectUtil#invoke(Object, String, Class, Object)} with {@code obj}, {@code methodName}, {@code argType}, {@code arg}.
   * <ul>
   *   <li>When {@code Obj}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReflectUtil#invoke(Object, String, Class, Object)}
   */
  @Test
  public void testInvokeWithObjMethodNameArgTypeArg_whenObj_thenThrowBuildException() {
    // Arrange
    Class<Object> argType = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> ReflectUtil.invoke("Obj", "Method Name", argType, "Arg"));
  }

  /**
   * Test {@link ReflectUtil#invoke(Object, String)} with {@code obj}, {@code methodName}.
   * <ul>
   *   <li>When {@code Obj}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReflectUtil#invoke(Object, String)}
   */
  @Test
  public void testInvokeWithObjMethodName_whenObj_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ReflectUtil.invoke("Obj", "Method Name"));
  }

  /**
   * Test {@link ReflectUtil#invokeStatic(Object, String)}.
   * <p>
   * Method under test: {@link ReflectUtil#invokeStatic(Object, String)}
   */
  @Test
  public void testInvokeStatic() {
    // Arrange
    Class<Object> forNameResult = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> ReflectUtil.invokeStatic(forNameResult, "Method Name"));
  }

  /**
   * Test {@link ReflectUtil#getField(Object, String)}.
   * <ul>
   *   <li>When {@code Obj}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReflectUtil#getField(Object, String)}
   */
  @Test
  public void testGetField_whenObj_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ReflectUtil.getField("Obj", "Field Name"));
  }

  /**
   * Test {@link ReflectUtil#throwBuildException(Exception)}.
   * <p>
   * Method under test: {@link ReflectUtil#throwBuildException(Exception)}
   */
  @Test
  public void testThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ReflectUtil.throwBuildException(new Exception("foo")));
  }

  /**
   * Test {@link ReflectUtil#toBuildException(Exception)}.
   * <p>
   * Method under test: {@link ReflectUtil#toBuildException(Exception)}
   */
  @Test
  public void testToBuildException() {
    // Arrange
    Exception t = new Exception("foo");

    // Act
    BuildException actualToBuildExceptionResult = ReflectUtil.toBuildException(t);

    // Assert
    assertEquals("java.lang.Exception: foo", actualToBuildExceptionResult.getLocalizedMessage());
    assertEquals("java.lang.Exception: foo", actualToBuildExceptionResult.getMessage());
    Location location = actualToBuildExceptionResult.getLocation();
    assertNull(location.getFileName());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualToBuildExceptionResult.getSuppressed().length);
    assertSame(t, actualToBuildExceptionResult.getCause());
    assertSame(t, actualToBuildExceptionResult.getException());
  }

  /**
   * Test {@link ReflectUtil#respondsTo(Object, String)}.
   * <p>
   * Method under test: {@link ReflectUtil#respondsTo(Object, String)}
   */
  @Test
  public void testRespondsTo() throws BuildException {
    // Arrange, Act and Assert
    assertFalse(ReflectUtil.respondsTo("42", "Method Name"));
  }
}
