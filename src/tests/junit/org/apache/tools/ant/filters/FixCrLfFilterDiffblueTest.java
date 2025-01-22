package org.apache.tools.ant.filters;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.FileDescriptor;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import org.apache.tools.ant.filters.FixCrLfFilter.AddAsisRemove;
import org.apache.tools.ant.filters.FixCrLfFilter.CrLf;
import org.junit.Test;

public class FixCrLfFilterDiffblueTest {
  /**
   * Test AddAsisRemove {@link AddAsisRemove#equals(Object)}, and {@link AddAsisRemove#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link AddAsisRemove#equals(Object)}
   *   <li>{@link AddAsisRemove#hashCode()}
   * </ul>
   */
  @Test
  public void testAddAsisRemoveEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    AddAsisRemove newInstanceResult = AddAsisRemove.newInstance("add");
    AddAsisRemove newInstanceResult2 = AddAsisRemove.newInstance("add");

    // Act and Assert
    assertEquals(newInstanceResult, newInstanceResult2);
    int expectedHashCodeResult = newInstanceResult.hashCode();
    assertEquals(expectedHashCodeResult, newInstanceResult2.hashCode());
  }

  /**
   * Test AddAsisRemove {@link AddAsisRemove#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link AddAsisRemove#equals(Object)}
   */
  @Test
  public void testAddAsisRemoveEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange
    AddAsisRemove newInstanceResult = AddAsisRemove.newInstance("remove");

    // Act and Assert
    assertNotEquals(newInstanceResult, AddAsisRemove.newInstance("add"));
  }

  /**
   * Test AddAsisRemove {@link AddAsisRemove#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link AddAsisRemove#equals(Object)}
   */
  @Test
  public void testAddAsisRemoveEquals_whenOtherIsDifferent_thenReturnNotEqual2() {
    // Arrange, Act and Assert
    assertNotEquals(AddAsisRemove.newInstance("add"), 1);
  }

  /**
   * Test AddAsisRemove {@link AddAsisRemove#getValues()}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code add} and {@code asis}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AddAsisRemove#getValues()}
   */
  @Test
  public void testAddAsisRemoveGetValues_thenReturnArrayOfStringWithAddAndAsis() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"add", "asis", "remove"}, AddAsisRemove.newInstance("add").getValues());
  }

  /**
   * Test AddAsisRemove new {@link AddAsisRemove} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link AddAsisRemove}
   */
  @Test
  public void testAddAsisRemoveNewAddAsisRemove() {
    // Arrange and Act
    AddAsisRemove actualAddAsisRemove = new AddAsisRemove();

    // Assert
    assertNull(actualAddAsisRemove.getValue());
    assertEquals(-1, actualAddAsisRemove.getIndex());
  }

  /**
   * Test AddAsisRemove {@link AddAsisRemove#newInstance(String)} with {@code String}.
   * <ul>
   *   <li>When {@code add}.</li>
   *   <li>Then return Value is {@code add}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AddAsisRemove#newInstance(String)}
   */
  @Test
  public void testAddAsisRemoveNewInstanceWithString_whenAdd_thenReturnValueIsAdd() {
    // Arrange and Act
    AddAsisRemove actualNewInstanceResult = AddAsisRemove.newInstance("add");

    // Assert
    assertEquals("add", actualNewInstanceResult.getValue());
    assertEquals(0, actualNewInstanceResult.getIndex());
    assertArrayEquals(new String[]{"add", "asis", "remove"}, actualNewInstanceResult.getValues());
  }

  /**
   * Test AddAsisRemove {@link AddAsisRemove#resolve()}.
   * <ul>
   *   <li>Given {@link AddAsisRemove} (default constructor).</li>
   *   <li>Then throw {@link IllegalStateException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AddAsisRemove#resolve()}
   */
  @Test
  public void testAddAsisRemoveResolve_givenAddAsisRemove_thenThrowIllegalStateException()
      throws IllegalStateException {
    // Arrange, Act and Assert
    assertThrows(IllegalStateException.class, () -> (new AddAsisRemove()).resolve());
  }

  /**
   * Test AddAsisRemove {@link AddAsisRemove#resolve()}.
   * <ul>
   *   <li>Given newInstance {@code add}.</li>
   *   <li>Then return newInstance {@code add}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AddAsisRemove#resolve()}
   */
  @Test
  public void testAddAsisRemoveResolve_givenNewInstanceAdd_thenReturnNewInstanceAdd() throws IllegalStateException {
    // Arrange
    AddAsisRemove newInstanceResult = AddAsisRemove.newInstance("add");

    // Act and Assert
    assertEquals(newInstanceResult, newInstanceResult.resolve());
  }

  /**
   * Test AddAsisRemove {@link AddAsisRemove#resolve()}.
   * <ul>
   *   <li>Given newInstance {@code remove}.</li>
   *   <li>Then return newInstance {@code remove}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AddAsisRemove#resolve()}
   */
  @Test
  public void testAddAsisRemoveResolve_givenNewInstanceRemove_thenReturnNewInstanceRemove()
      throws IllegalStateException {
    // Arrange
    AddAsisRemove newInstanceResult = AddAsisRemove.newInstance("remove");

    // Act and Assert
    assertEquals(newInstanceResult, newInstanceResult.resolve());
  }

  /**
   * Test CrLf {@link CrLf#equals(Object)}, and {@link CrLf#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CrLf#equals(Object)}
   *   <li>{@link CrLf#hashCode()}
   * </ul>
   */
  @Test
  public void testCrLfEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    CrLf newInstanceResult = CrLf.newInstance("mac");
    CrLf newInstanceResult2 = CrLf.newInstance("mac");

    // Act and Assert
    assertEquals(newInstanceResult, newInstanceResult2);
    int expectedHashCodeResult = newInstanceResult.hashCode();
    assertEquals(expectedHashCodeResult, newInstanceResult2.hashCode());
  }

  /**
   * Test CrLf {@link CrLf#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link CrLf#equals(Object)}
   */
  @Test
  public void testCrLfEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange
    CrLf newInstanceResult = CrLf.newInstance("unix");

    // Act and Assert
    assertNotEquals(newInstanceResult, CrLf.newInstance("mac"));
  }

  /**
   * Test CrLf {@link CrLf#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link CrLf#equals(Object)}
   */
  @Test
  public void testCrLfEquals_whenOtherIsDifferent_thenReturnNotEqual2() {
    // Arrange, Act and Assert
    assertNotEquals(CrLf.newInstance("mac"), 1);
  }

  /**
   * Test CrLf {@link CrLf#getValues()}.
   * <ul>
   *   <li>Given newInstance {@code mac}.</li>
   *   <li>Then return array of {@link String} with {@code asis} and {@code cr}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CrLf#getValues()}
   */
  @Test
  public void testCrLfGetValues_givenNewInstanceMac_thenReturnArrayOfStringWithAsisAndCr() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"asis", "cr", "lf", "crlf", "mac", "unix", "dos"},
        CrLf.newInstance("mac").getValues());
  }

  /**
   * Test CrLf new {@link CrLf} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CrLf}
   */
  @Test
  public void testCrLfNewCrLf() {
    // Arrange and Act
    CrLf actualCrLf = new CrLf();

    // Assert
    assertNull(actualCrLf.getValue());
    assertEquals(-1, actualCrLf.getIndex());
  }

  /**
   * Test CrLf {@link CrLf#newInstance(String)} with {@code String}.
   * <ul>
   *   <li>When {@code asis}.</li>
   *   <li>Then return Value is {@code asis}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CrLf#newInstance(String)}
   */
  @Test
  public void testCrLfNewInstanceWithString_whenAsis_thenReturnValueIsAsis() {
    // Arrange and Act
    CrLf actualNewInstanceResult = CrLf.newInstance("asis");

    // Assert
    assertEquals("asis", actualNewInstanceResult.getValue());
    assertEquals(0, actualNewInstanceResult.getIndex());
    assertArrayEquals(new String[]{"asis", "cr", "lf", "crlf", "mac", "unix", "dos"},
        actualNewInstanceResult.getValues());
  }

  /**
   * Test CrLf {@link CrLf#resolve()}.
   * <ul>
   *   <li>Given {@link CrLf} (default constructor).</li>
   *   <li>Then throw {@link IllegalStateException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CrLf#resolve()}
   */
  @Test
  public void testCrLfResolve_givenCrLf_thenThrowIllegalStateException() {
    // Arrange, Act and Assert
    assertThrows(IllegalStateException.class, () -> (new CrLf()).resolve());
  }

  /**
   * Test CrLf {@link CrLf#resolve()}.
   * <ul>
   *   <li>Given newInstance {@code mac}.</li>
   *   <li>Then return Value is {@code cr}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CrLf#resolve()}
   */
  @Test
  public void testCrLfResolve_givenNewInstanceMac_thenReturnValueIsCr() {
    // Arrange and Act
    CrLf actualResolveResult = CrLf.newInstance("mac").resolve();

    // Assert
    assertEquals("cr", actualResolveResult.getValue());
    assertEquals(1, actualResolveResult.getIndex());
    assertArrayEquals(new String[]{"asis", "cr", "lf", "crlf", "mac", "unix", "dos"}, actualResolveResult.getValues());
  }

  /**
   * Test CrLf {@link CrLf#resolve()}.
   * <ul>
   *   <li>Given newInstance {@code unix}.</li>
   *   <li>Then return Value is {@code lf}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CrLf#resolve()}
   */
  @Test
  public void testCrLfResolve_givenNewInstanceUnix_thenReturnValueIsLf() {
    // Arrange and Act
    CrLf actualResolveResult = CrLf.newInstance("unix").resolve();

    // Assert
    assertEquals("lf", actualResolveResult.getValue());
    assertEquals(2, actualResolveResult.getIndex());
    assertArrayEquals(new String[]{"asis", "cr", "lf", "crlf", "mac", "unix", "dos"}, actualResolveResult.getValues());
  }

  /**
   * Test {@link FixCrLfFilter#FixCrLfFilter()}.
   * <p>
   * Method under test: {@link FixCrLfFilter#FixCrLfFilter()}
   */
  @Test
  public void testNewFixCrLfFilter() {
    // Arrange and Act
    FixCrLfFilter actualFixCrLfFilter = new FixCrLfFilter();

    // Assert
    AddAsisRemove tab = actualFixCrLfFilter.getTab();
    assertEquals("asis", tab.getValue());
    CrLf eol = actualFixCrLfFilter.getEol();
    assertEquals("lf", eol.getValue());
    AddAsisRemove eof = actualFixCrLfFilter.getEof();
    assertEquals("remove", eof.getValue());
    assertNull(actualFixCrLfFilter.getParameters());
    assertNull(actualFixCrLfFilter.getProject());
    assertEquals(1, tab.getIndex());
    assertEquals(2, eof.getIndex());
    assertEquals(2, eol.getIndex());
    assertEquals(3, eof.getValues().length);
    assertEquals(3, tab.getValues().length);
    assertEquals(7, eol.getValues().length);
    assertEquals(8, actualFixCrLfFilter.getTablength());
    assertFalse(actualFixCrLfFilter.getInitialized());
    assertFalse(actualFixCrLfFilter.getJavafiles());
    assertTrue(actualFixCrLfFilter.getFixlast());
  }

  /**
   * Test {@link FixCrLfFilter#chain(Reader)}.
   * <ul>
   *   <li>Given {@link FixCrLfFilter#FixCrLfFilter()} Eof is newInstance {@code add}.</li>
   *   <li>Then return Eof is newInstance {@code add}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCrLfFilter#chain(Reader)}
   */
  @Test
  public void testChain_givenFixCrLfFilterEofIsNewInstanceAdd_thenReturnEofIsNewInstanceAdd() throws IOException {
    // Arrange
    FixCrLfFilter fixCrLfFilter = new FixCrLfFilter();
    AddAsisRemove attr = AddAsisRemove.newInstance("add");
    fixCrLfFilter.setEof(attr);

    // Act
    Reader actualChainResult = fixCrLfFilter.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof FixCrLfFilter);
    assertEquals("foo\n\u001a", ((FixCrLfFilter) actualChainResult).readFully());
    assertEquals(attr, ((FixCrLfFilter) actualChainResult).getEof());
  }

  /**
   * Test {@link FixCrLfFilter#chain(Reader)}.
   * <ul>
   *   <li>Given {@link FixCrLfFilter#FixCrLfFilter()} Eol is newInstance {@code mac}.</li>
   *   <li>Then return Eol Value is {@code cr}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCrLfFilter#chain(Reader)}
   */
  @Test
  public void testChain_givenFixCrLfFilterEolIsNewInstanceMac_thenReturnEolValueIsCr() throws IOException {
    // Arrange
    FixCrLfFilter fixCrLfFilter = new FixCrLfFilter();
    fixCrLfFilter.setEol(CrLf.newInstance("mac"));

    // Act
    Reader actualChainResult = fixCrLfFilter.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof FixCrLfFilter);
    AddAsisRemove tab = ((FixCrLfFilter) actualChainResult).getTab();
    assertEquals("asis", tab.getValue());
    CrLf eol = ((FixCrLfFilter) actualChainResult).getEol();
    assertEquals("cr", eol.getValue());
    assertEquals("foo\r", ((FixCrLfFilter) actualChainResult).readFully());
    assertEquals(1, eol.getIndex());
    assertEquals(1, tab.getIndex());
  }

  /**
   * Test {@link FixCrLfFilter#chain(Reader)}.
   * <ul>
   *   <li>Given {@link FixCrLfFilter#FixCrLfFilter()} Javafiles is {@code true}.</li>
   *   <li>Then return Javafiles.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCrLfFilter#chain(Reader)}
   */
  @Test
  public void testChain_givenFixCrLfFilterJavafilesIsTrue_thenReturnJavafiles() {
    // Arrange
    FixCrLfFilter fixCrLfFilter = new FixCrLfFilter();
    fixCrLfFilter.setJavafiles(true);
    AddAsisRemove attr = AddAsisRemove.newInstance("add");
    fixCrLfFilter.setTab(attr);

    // Act
    Reader actualChainResult = fixCrLfFilter.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof FixCrLfFilter);
    assertTrue(((FixCrLfFilter) actualChainResult).getJavafiles());
    assertEquals(attr, ((FixCrLfFilter) actualChainResult).getTab());
  }

  /**
   * Test {@link FixCrLfFilter#chain(Reader)}.
   * <ul>
   *   <li>Given {@link FixCrLfFilter#FixCrLfFilter()} Tab is newInstance {@code add}.</li>
   *   <li>Then return Tab is newInstance {@code add}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCrLfFilter#chain(Reader)}
   */
  @Test
  public void testChain_givenFixCrLfFilterTabIsNewInstanceAdd_thenReturnTabIsNewInstanceAdd() throws IOException {
    // Arrange
    FixCrLfFilter fixCrLfFilter = new FixCrLfFilter();
    AddAsisRemove attr = AddAsisRemove.newInstance("add");
    fixCrLfFilter.setTab(attr);

    // Act
    Reader actualChainResult = fixCrLfFilter.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof FixCrLfFilter);
    assertEquals("foo\n", ((FixCrLfFilter) actualChainResult).readFully());
    assertEquals(attr, ((FixCrLfFilter) actualChainResult).getTab());
  }

  /**
   * Test {@link FixCrLfFilter#chain(Reader)}.
   * <ul>
   *   <li>Given {@link FixCrLfFilter#FixCrLfFilter()}.</li>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return not Javafiles.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCrLfFilter#chain(Reader)}
   */
  @Test
  public void testChain_givenFixCrLfFilter_whenStringReaderWithFoo_thenReturnNotJavafiles() throws IOException {
    // Arrange
    FixCrLfFilter fixCrLfFilter = new FixCrLfFilter();

    // Act
    Reader actualChainResult = fixCrLfFilter.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof FixCrLfFilter);
    AddAsisRemove tab = ((FixCrLfFilter) actualChainResult).getTab();
    assertEquals("asis", tab.getValue());
    assertEquals("foo\n", ((FixCrLfFilter) actualChainResult).readFully());
    assertEquals(1, tab.getIndex());
    assertFalse(((FixCrLfFilter) actualChainResult).getJavafiles());
  }

  /**
   * Test {@link FixCrLfFilter#chain(Reader)}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with {@code mac}.</li>
   *   <li>Then return not ready.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCrLfFilter#chain(Reader)}
   */
  @Test
  public void testChain_givenStringReaderWithMac_thenReturnNotReady() throws IOException {
    // Arrange
    FixCrLfFilter fixCrLfFilter = new FixCrLfFilter(new StringReader("mac"));

    // Act
    Reader actualChainResult = fixCrLfFilter.chain(new FileReader(new FileDescriptor()));

    // Assert
    assertTrue(actualChainResult instanceof FixCrLfFilter);
    AddAsisRemove tab = ((FixCrLfFilter) actualChainResult).getTab();
    assertEquals("asis", tab.getValue());
    assertEquals(1, tab.getIndex());
    assertFalse(actualChainResult.ready());
    assertFalse(((FixCrLfFilter) actualChainResult).getJavafiles());
  }

  /**
   * Test {@link FixCrLfFilter#chain(Reader)}.
   * <ul>
   *   <li>Then return Eof is newInstance {@code remove}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCrLfFilter#chain(Reader)}
   */
  @Test
  public void testChain_thenReturnEofIsNewInstanceRemove() {
    // Arrange
    FixCrLfFilter fixCrLfFilter = new FixCrLfFilter();
    fixCrLfFilter.setJavafiles(true);
    AddAsisRemove attr = AddAsisRemove.newInstance("remove");
    fixCrLfFilter.setTab(attr);

    // Act
    Reader actualChainResult = fixCrLfFilter.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof FixCrLfFilter);
    assertEquals(attr, ((FixCrLfFilter) actualChainResult).getEof());
    assertEquals(attr, ((FixCrLfFilter) actualChainResult).getTab());
  }

  /**
   * Test {@link FixCrLfFilter#getEof()}.
   * <p>
   * Method under test: {@link FixCrLfFilter#getEof()}
   */
  @Test
  public void testGetEof() {
    // Arrange and Act
    AddAsisRemove actualEof = (new FixCrLfFilter()).getEof();

    // Assert
    assertEquals("remove", actualEof.getValue());
    assertEquals(2, actualEof.getIndex());
    assertArrayEquals(new String[]{"add", "asis", "remove"}, actualEof.getValues());
  }

  /**
   * Test {@link FixCrLfFilter#getEol()}.
   * <p>
   * Method under test: {@link FixCrLfFilter#getEol()}
   */
  @Test
  public void testGetEol() {
    // Arrange and Act
    CrLf actualEol = (new FixCrLfFilter()).getEol();

    // Assert
    assertEquals("lf", actualEol.getValue());
    assertEquals(2, actualEol.getIndex());
    assertArrayEquals(new String[]{"asis", "cr", "lf", "crlf", "mac", "unix", "dos"}, actualEol.getValues());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link FixCrLfFilter#setFixlast(boolean)}
   *   <li>{@link FixCrLfFilter#setJavafiles(boolean)}
   *   <li>{@link FixCrLfFilter#getFixlast()}
   *   <li>{@link FixCrLfFilter#getJavafiles()}
   *   <li>{@link FixCrLfFilter#getTablength()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    FixCrLfFilter fixCrLfFilter = new FixCrLfFilter();

    // Act
    fixCrLfFilter.setFixlast(true);
    fixCrLfFilter.setJavafiles(true);
    boolean actualFixlast = fixCrLfFilter.getFixlast();
    boolean actualJavafiles = fixCrLfFilter.getJavafiles();

    // Assert
    assertEquals(8, fixCrLfFilter.getTablength());
    assertTrue(actualFixlast);
    assertTrue(actualJavafiles);
  }

  /**
   * Test {@link FixCrLfFilter#getTab()}.
   * <p>
   * Method under test: {@link FixCrLfFilter#getTab()}
   */
  @Test
  public void testGetTab() {
    // Arrange and Act
    AddAsisRemove actualTab = (new FixCrLfFilter()).getTab();

    // Assert
    assertEquals("asis", actualTab.getValue());
    assertEquals(1, actualTab.getIndex());
    assertArrayEquals(new String[]{"add", "asis", "remove"}, actualTab.getValues());
  }

  /**
   * Test {@link FixCrLfFilter#read()}.
   * <ul>
   *   <li>Given {@link FixCrLfFilter#FixCrLfFilter(Reader)} with in is {@link StringReader#StringReader(String)} skip sixteen.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCrLfFilter#read()}
   */
  @Test
  public void testRead_givenFixCrLfFilterWithInIsStringReaderSkipSixteen_thenReturnMinusOne()
      throws IOException, IllegalArgumentException {
    // Arrange
    FixCrLfFilter fixCrLfFilter = new FixCrLfFilter(new StringReader("\n"));
    fixCrLfFilter.skip(16L);

    // Act and Assert
    assertEquals(-1, fixCrLfFilter.read());
  }

  /**
   * Test {@link FixCrLfFilter#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return ten.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCrLfFilter#read()}
   */
  @Test
  public void testRead_givenStringReaderWithEmptyString_thenReturnTen() throws IOException {
    // Arrange, Act and Assert
    assertEquals(10, (new FixCrLfFilter(new StringReader(""))).read());
  }

  /**
   * Test {@link FixCrLfFilter#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCrLfFilter#read()}
   */
  @Test
  public void testRead_givenStringReaderWithFoo_thenReturnOneHundredTwo() throws IOException {
    // Arrange, Act and Assert
    assertEquals(102, (new FixCrLfFilter(new StringReader("foo"))).read());
  }

  /**
   * Test {@link FixCrLfFilter#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with lf.</li>
   *   <li>Then return ten.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCrLfFilter#read()}
   */
  @Test
  public void testRead_givenStringReaderWithLf_thenReturnTen() throws IOException {
    // Arrange, Act and Assert
    assertEquals(10, (new FixCrLfFilter(new StringReader("\n"))).read());
  }

  /**
   * Test {@link FixCrLfFilter#setEof(AddAsisRemove)}.
   * <ul>
   *   <li>When newInstance {@code add}.</li>
   *   <li>Then {@link FixCrLfFilter#FixCrLfFilter()} Eof is newInstance {@code add}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCrLfFilter#setEof(AddAsisRemove)}
   */
  @Test
  public void testSetEof_whenNewInstanceAdd_thenFixCrLfFilterEofIsNewInstanceAdd() {
    // Arrange
    FixCrLfFilter fixCrLfFilter = new FixCrLfFilter();
    AddAsisRemove attr = AddAsisRemove.newInstance("add");

    // Act
    fixCrLfFilter.setEof(attr);

    // Assert
    assertEquals(attr, fixCrLfFilter.getEof());
  }

  /**
   * Test {@link FixCrLfFilter#setEof(AddAsisRemove)}.
   * <ul>
   *   <li>When newInstance {@code remove}.</li>
   *   <li>Then {@link FixCrLfFilter#FixCrLfFilter()} Eof is newInstance {@code remove}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCrLfFilter#setEof(AddAsisRemove)}
   */
  @Test
  public void testSetEof_whenNewInstanceRemove_thenFixCrLfFilterEofIsNewInstanceRemove() {
    // Arrange
    FixCrLfFilter fixCrLfFilter = new FixCrLfFilter();
    AddAsisRemove attr = AddAsisRemove.newInstance("remove");

    // Act
    fixCrLfFilter.setEof(attr);

    // Assert that nothing has changed
    assertEquals(attr, fixCrLfFilter.getEof());
  }

  /**
   * Test {@link FixCrLfFilter#setEol(CrLf)}.
   * <ul>
   *   <li>When newInstance {@code mac}.</li>
   *   <li>Then {@link FixCrLfFilter#FixCrLfFilter()} Eol Value is {@code cr}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCrLfFilter#setEol(CrLf)}
   */
  @Test
  public void testSetEol_whenNewInstanceMac_thenFixCrLfFilterEolValueIsCr() {
    // Arrange
    FixCrLfFilter fixCrLfFilter = new FixCrLfFilter();

    // Act
    fixCrLfFilter.setEol(CrLf.newInstance("mac"));

    // Assert
    CrLf eol = fixCrLfFilter.getEol();
    assertEquals("cr", eol.getValue());
    assertEquals(1, eol.getIndex());
  }

  /**
   * Test {@link FixCrLfFilter#setEol(CrLf)}.
   * <ul>
   *   <li>When newInstance {@code unix}.</li>
   *   <li>Then {@link FixCrLfFilter#FixCrLfFilter()} Eol Value is {@code lf}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCrLfFilter#setEol(CrLf)}
   */
  @Test
  public void testSetEol_whenNewInstanceUnix_thenFixCrLfFilterEolValueIsLf() {
    // Arrange
    FixCrLfFilter fixCrLfFilter = new FixCrLfFilter();

    // Act
    fixCrLfFilter.setEol(CrLf.newInstance("unix"));

    // Assert that nothing has changed
    CrLf eol = fixCrLfFilter.getEol();
    assertEquals("lf", eol.getValue());
    assertEquals(2, eol.getIndex());
  }

  /**
   * Test {@link FixCrLfFilter#setTab(AddAsisRemove)}.
   * <ul>
   *   <li>When newInstance {@code add}.</li>
   *   <li>Then {@link FixCrLfFilter#FixCrLfFilter()} Tab is newInstance {@code add}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCrLfFilter#setTab(AddAsisRemove)}
   */
  @Test
  public void testSetTab_whenNewInstanceAdd_thenFixCrLfFilterTabIsNewInstanceAdd() {
    // Arrange
    FixCrLfFilter fixCrLfFilter = new FixCrLfFilter();
    AddAsisRemove attr = AddAsisRemove.newInstance("add");

    // Act
    fixCrLfFilter.setTab(attr);

    // Assert
    assertEquals(attr, fixCrLfFilter.getTab());
  }

  /**
   * Test {@link FixCrLfFilter#setTab(AddAsisRemove)}.
   * <ul>
   *   <li>When newInstance {@code remove}.</li>
   *   <li>Then {@link FixCrLfFilter#FixCrLfFilter()} Tab is newInstance {@code remove}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCrLfFilter#setTab(AddAsisRemove)}
   */
  @Test
  public void testSetTab_whenNewInstanceRemove_thenFixCrLfFilterTabIsNewInstanceRemove() {
    // Arrange
    FixCrLfFilter fixCrLfFilter = new FixCrLfFilter();
    AddAsisRemove attr = AddAsisRemove.newInstance("remove");

    // Act
    fixCrLfFilter.setTab(attr);

    // Assert
    assertEquals(attr, fixCrLfFilter.getTab());
  }

  /**
   * Test {@link FixCrLfFilter#setTablength(int)}.
   * <ul>
   *   <li>When eighty-one.</li>
   *   <li>Then throw {@link IOException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCrLfFilter#setTablength(int)}
   */
  @Test
  public void testSetTablength_whenEightyOne_thenThrowIOException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class, () -> (new FixCrLfFilter()).setTablength(81));
  }

  /**
   * Test {@link FixCrLfFilter#setTablength(int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then throw {@link IOException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCrLfFilter#setTablength(int)}
   */
  @Test
  public void testSetTablength_whenOne_thenThrowIOException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class, () -> (new FixCrLfFilter()).setTablength(1));
  }

  /**
   * Test {@link FixCrLfFilter#setTablength(int)}.
   * <ul>
   *   <li>When three.</li>
   *   <li>Then {@link FixCrLfFilter#FixCrLfFilter()} Tablength is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link FixCrLfFilter#setTablength(int)}
   */
  @Test
  public void testSetTablength_whenThree_thenFixCrLfFilterTablengthIsThree() throws IOException {
    // Arrange
    FixCrLfFilter fixCrLfFilter = new FixCrLfFilter();

    // Act
    fixCrLfFilter.setTablength(3);

    // Assert
    assertEquals(3, fixCrLfFilter.getTablength());
  }
}
